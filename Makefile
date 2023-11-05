##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

STACK = stack

VM_PROJECT_DIR = Vm

all: $(NAME)
	$(MAKE) -C $(VM_PROJECT_DIR) -f Makefile

$(NAME):
	$(STACK) install

clean:
	$(STACK) clean
	cd Vm && $(MAKE) -f Makefile clean

fclean: clean
	$(RM) -fr $(NAME)
	$(STACK) purge
	$(RM) -fr docs/haddock
	$(RM) -fr lip.lop
	cd Vm && $(MAKE) -f Makefile fclean


re: fclean all
	cd Vm && $(MAKE) -f Makefile re

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
