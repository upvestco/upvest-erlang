REBAR3 ?= $(shell which rebar3)
ELVIS ?= $(shell which elvis)


all: $(REBAR3)
	@$(REBAR3) do clean, compile, eunit

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

coveralls:
	@echo "Running rebar3 coveralls send..."
	@$(REBAR3) as test coveralls send

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR3) dialyzer

edoc:
	@echo "Running rebar3 edoc..."
	@$(REBAR3) as edoc edoc

ct:
	@echo "Running rebar3 common test suite..."
	@$(REBAR3) ct --logdir=./test/logs

test: elvis ct

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock -V

travis: test coveralls

shell:
	@$(REBAR3) as shell shell

FMT = _build/erlang-formatter-master/fmt.sh
$(FMT):
	@mkdir -p _build/
	@curl -f#SL 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/master' | tar xvz -C _build/

# Pick either this one to go through the whole project
fmt: TO_FMT ?= .

fmt: $(FMT)
	@$(if $(TO_FMT), $(FMT) $(TO_FMT))

.PHONY: all clean compile test eunit xref coveralls edoc shell dialyzer rebar3 fmt
