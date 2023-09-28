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
	rm -f $(NAME)
	$(STACK) clean

fclean: clean
	$(STACK) purge

re: fclean all

test_run:
	$(STACK) test

coverage:
	$(STACK) test --coverage

fclean_test: fclean
	$(STACK) purge --test

doc:
	$(STACK) haddock --odir=doc/haddock

.phony: all clean fclean re test_run coverage fclean_test doc
