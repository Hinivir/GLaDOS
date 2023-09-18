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

.phony: all clean fclean re
