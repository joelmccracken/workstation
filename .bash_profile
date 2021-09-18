export PATH="/usr/local/bin:$PATH"


export EDITOR=emacsclient
export GIT_EDITOR=$EDITOR

alias lock=/System/Library/Frameworks/ScreenSaver.framework/Versions/Current/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine

export PATH=/usr/local/opt/texinfo/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=$HOME/.emacs.d/bin:$PATH
# add emacs
export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH
# add emacsclient, etc
export PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=~/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/ttm/devtools/bin:$PATH
export PATH="/opt/homebrew-cask/Caskroom/racket/6.1.1/Racket\ v6.1.1/bin/raco:$PATH"

# export PATH="/usr/local/Cellar/ruby20/2.0.0-p481/bin:$PATH"

export LANG=en_US.UTF-8

git-on-branch () {
    git stash
    ORIGINAL_BRANCH=`git branch | grep \* | sed 's/\*[[:space:]]//'`
    git checkout $1
    $2
    git checkout $ORIGINAL_BRANCH
    git stash pop
}

shopt -s extglob


# instead of this, use the ssh key config
# {
#     cd ~/secrets;
#     ssh-add `ls | grep id_rsa | grep -v .pub` > /dev/null 2>&1;
#     cd - > /dev/null 2>&1
# }

function aalias {
    mkdir -p ~/.bash_it/custom/
    echo "alias ${1}='${@:2}'" >> ~/.bash_it/custom/aliases.bash
    source ~/.bash_it/custom/aliases.bash
}

function on-branch {
    local original_branch=$(git branch | sed -n '/\* /s///p')
    git checkout $1 && \
        bash && \
        git checkout $original_branch
}

function alerts_prompt {
    wc -l ~/.redd-up/cache | awk '{print $1}'
}

function alerts {
    cat ~/.redd-up/cache
}

function jnm_prompt_command {
    PS1="\n$(alerts_prompt) ${yellow}$(ruby_version_prompt) ${purple}\h ${reset_color}in ${green}\w\n${bold_cyan}$(scm_char)${green}$(scm_prompt_info) ${green}→${reset_color} "
}
# PROMPT_COMMAND=jnm_prompt_command;

function save(){ echo "$@" >> ~/var/saved_commands; }

export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend

upsearch () {
    test / == "$PWD" && return || test -e "$1" && echo $PWD && return || cd .. && upsearch "$1"
}

function pathmagic {
    i=$1
    eval "
function $i {
    DIR=`upsearch bin`

    if [[ -e \"\$DIR/bin/$i\" ]];
    then
        path="\$DIR/bin/$i"
        \"\$path\" \"\$@\"
    else
        command $i \"\$@\"
    fi
}
"
}


# for i in rails m rake ttmscalr foreman;
# do
#     pathmagic $i
# done

do_command_done_alert() {
    osascript -e 'display dialog "Command Done!"'
}

alert_when_done() {
    if test -n "$1";
    then
        while kill -0 $1
        do
            sleep 1
        done
    fi
    do_command_done_alert
}

replace_in_ruby_files(){
    LANG=UTF-8 find . -name '*.rb' -not -path './.gems/*' -exec ruby -p -i -e "gsub(/$1/, '$2')" {} \; ;
}

replace_in_ruby_files2(){
    LANG=UTF-8 ruby -p -i -e "gsub(/$1/, '$2')"
}

delete_old_branches() {
    git branch --merged rc | grep -v "\* rc" | xargs -n 1 git branch -d
}

bash_simple_prompt() {
    PS1=\$\  bash
}

alias stack-shell="stack exec --no-ghc-package-path bash"

m_on_each() {
    while read file;
    do
        if [[ -f "$file" ]];
        then
            bundle exec m "$file" || exit $?;
        fi;
    done
}

find_test_files_matching() {
    find . -path './test/*_test.rb' "$@"
}

cdsc() {
    cd ~/showclix/showclix/home/showclix;
}

rvm_script="$HOME/.rvm/scripts/rvm"

if [ -e "$rvm_script" ]; then
    source "$rvm_script";
fi

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/opt/node@8/bin:$PATH"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
if [ -e /Users/joel/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/joel/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
