#!/bin/bash

RED_COLOR='\033[0;31m'
GREEN_COLOR='\033[0;92m'
BLUE_COLOR='\033[0;94m'
NC='\033[0m'

# Message type setup
ERROR="${RED_COLOR}-- ERROR:"
SUCCESS="${GREEN_COLOR}-- SUCCESS:"
NOTE="${BLUE_COLOR}-- NOTE:"

TYPE=${SHELL##*/}
ALIAS="alias macs='emacs -nw'"
ZSH=".zshrc"
BASH=".bashrc"

function print_error() {
    echo -e "${ERROR} $1${NC}"
}

function print_note() {
    echo -e "${NOTE} $1${NC}"    
}

function print_success() {
    echo -e "${SUCCESS} $1${NC}"
	exit 1
}

function secure_append_alias() {
    local alias_find=$(grep -om1 "$ALIAS" ~/$1)
    if [ "$ALIAS" != "$alias_find" ]; then
        echo "$ALIAS" >> ~/$1
        print_success "alias added for ${TYPE} shell"
    else
        print_note "alias already added for ${TYPE} shell"
    fi
}

if [ "$TYPE" = "zsh" ]; then
    secure_append_alias ".zshrc"
elif [ "$TYPE" = "bash" ]; then
    secure_append_alias ".bashrc"
else
    print_erros "Can't add alias for ${TYPE} shell"
fi


# Deploy enviroment 
print_note "Deploying enviroment"
cd ~/.emacs.d

# Pulling submodelus
print_note "Fetching git submodules"
# Disabled because no git submodules are in use for a while
# git submodule update --init --recursive

# Install fonts
print_note "Installing fonts"
cp fonts/FiraCodeNerdFontMono-Regular.ttf ~/.local/share/fonts/.

cd ~/

print_success "Deploy finished. Restart your shell to apply aliases"

# Reloading config files and finishing.
## Disabled because of oh-my-zsh unsupport
# if [ "$TYPE" = "zsh" ]; then
#     source ~/$ZSH
#     print_success "Deploy finished"
# elif [ "$TYPE" = "bash" ]; then
#     source ~/$BASH
#     print_success "Deploy finished"
# else
#     print_erros "No special support for ${TYPE} shell"
# fi
