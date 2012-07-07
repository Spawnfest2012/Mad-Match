ERL := erl -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -s crypto -s inets  -s ssl -s lager -s emysql  -s ibrowse  ${ERL_ARGS}

all:
	rebar get-deps && rebar compile 

clean:
	rebar clean

build_plt: all
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets xmerl webtool snmp public_key mnesia syntax_tools compiler --output_plt pingterest.plt -pa deps/*/ebin ebin

analyze: all
	dialyzer --verbose -pa deps/*/ebin --plt pingterest.plt -Wunmatched_returns -Werror_handling ebin

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

test: all
	mkdir -p ./log
	mkdir -p ./log/ct
	rebar skip_deps=true ct ${CT_ARGS}; \

shell: all
	if [ -f config/`hostname`.config ]; then \
		${ERL} -config config/`hostname` -boot start_sasl; \
	else \
		${ERL} -boot start_sasl; \
	fi

run: all
	${ERL} -boot start_sasl -s pingterest; \