##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

STACK = stack

all: $(NAME)

$(NAME):
	$(STACK) install

clean:
	$(STACK) clean

fclean: clean
	$(RM) -fr $(NAME)
	$(STACK) purge
	$(RM) -fr docs/haddock

re: fclean all

test_run:
	$(STACK) test

coverage:
	$(STACK) test --coverage

fclean_test: fclean
	$(STACK) purge --test

doc:
	$(STACK) haddock --haddock-arguments "--odir=docs/haddock"

.phony: all clean fclean re test_run coverage fclean_test doc
