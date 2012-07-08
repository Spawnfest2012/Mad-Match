set :rvm_type, :user
$:.unshift(File.expand_path('./lib', ENV['rvm_path'])) # Add RVM's lib directory to the load path.
require "rvm/capistrano"                        # Load RVM's capistrano plugin.
require 'open-uri'

set :rvm_ruby_string, 'ruby-1.9.2-p290@pingterest' # Or whatever env you want it to run in.
set :keep_releases,       5
set :application,         "pingterest"
set :user,                "pingterest"
set :deploy_to,           "/var/web/#{application}"
set :use_sudo,            false
set :term,                "linux"
set :deploy_via,          :copy

ssh_options[:forward_agent] = true
ssh_options[:port] = 22

if ENV['BUILD_NUMBER']
  $requested = ENV['BUILD_NUMBER']
else
  $requested = Capistrano::CLI.ui.ask("THIS IS PINGTEREST PRODUCTION. Enter build number to deploy:")
end
exit unless $requested.to_i > 30

role :prod, "pingterest.inakalabs.com"

namespace :deploy do

  # remove rails-focused deploy tasks
  [:finalize_update, :update_code].each do |default_task|
    task default_task do 
      # ... ahh, silence!
    end
  end

  namespace :pingterest do

    task :build, :roles => :prod, :except => {:no_release => true} do
      puts "downloading..."
      run "cd #{releases_path}; wget -q \"http://build.inakalabs.com/job/spawnfest2012/#{$requested}/artifact/*zip*/archive.zip\" -O /tmp/archive#{$requested}.zip"
      run "mkdir -p #{release_path}"
      puts "unarchiving..."
      run "cd #{release_path}; unzip -o /tmp/archive#{$requested}.zip 2>&1"
      run "mv #{release_path}/archive/* #{release_path}; rm -rf #{release_path}/archive"
      run "rm #{current_path} || true"
      run "ln -s #{release_path} #{current_path}"

           # in the build machine environment swap out the version #
      run "cd #{current_path};sed 's/0\.0\.0/0.0.#{$requested}/' < src/pingterest.app.src > src/pingterest.app.src2; mv src/pingterest.app.src2 src/pingterest.app.src"
      run "cd #{current_path};sed \"s/0\.0\.0/0.0.#{$requested} ($HOSTNAME)/\" < src/ping_web.erl > src/ping_web.erl2; mv src/ping_web.erl2 src/ping_web.erl"
      run "make clean && chmod +x deps/erlsha2/c_src/config.sh && make"

      run "cd #{current_path}; cp -f config/erlang/production.config config/erlang/pingterest.config"
    end

      task :announce do
        OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE
        open("https://api.hipchat.com/v1/rooms/message?from=Jenkins&auth_token=f9a98545975dc67294a41cd975860a&room_id=Spawnfest2012&color=red&message=deploying+production+build+#{$requested}").read
      end

    end

    task :restart_erlang do
      run "sudo /etc/init.d/pingterest stop"
      run "sudo /etc/init.d/pingterest start"
    end
end

deploy.task :restart, :roles => :prod  do 
  deploy.pingterest.restart_erlang
end

after "deploy:create_symlink", "deploy:pingterest:build"
after "deploy", "deploy:cleanup"
before "deploy", "deploy:pingterest:announce"

def serialize_task_for(task_name, servers)
  servers.each do |server|
    puts "    Performing #{task_name} for #{server} at #{Time.now}..."
    task(task_name, :hosts => server) do
      yield server
    end
    eval(task_name)
  end
end



