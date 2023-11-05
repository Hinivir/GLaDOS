##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

STACK = stack

all: $(NAME)
	$(MAKE) -C vm -f Makefile

$(NAME):
	$(STACK) install

clean:
	$(STACK) clean
	cd vm && $(MAKE) -f Makefile clean

fclean: clean
	$(RM) -fr $(NAME)
	$(STACK) purge
	$(RM) -fr docs/haddock
	$(RM) -fr lip.lop
	cd vm && $(MAKE) -f Makefile fclean


re: fclean all
	cd vm && $(MAKE) -f Makefile re

test_run:
	$(STACK) test

coverage:
	$(STACK) test --coverage

functional_test: re
	chmod +x ./test/functional_test.sh
	./test/functional_test.sh

fclean_test: fclean
	$(STACK) purge --test

doc:
	$(STACK) haddock --haddock-arguments "--odir=docs/haddock"

.phony: all clean fclean re test_run coverage functional_test fclean_test doc
