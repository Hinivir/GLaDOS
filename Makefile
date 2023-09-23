##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

all: $(NAME)

$(NAME):
	stack install

clean:
	rm -f $(NAME)
	stack clean

fclean: clean
	stack purge

re: fclean all

test_run:
	stack test

coverage:
	stack test --coverage

fclean_test: fclean
	stack purge --test

doc:
	stack haddock --odir=doc/haddock

.phony: all clean fclean re test_run coverage fclean_test doc
