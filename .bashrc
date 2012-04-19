#!/usr/bin/env bash
 
# Formatting constants
export JBOSS_HOME=/home/mharvan/jboss/jboss-4.2.3.GA
#export JBOSS_HOME=/ramdisk/jboss-4.2.3.GA
export SHELL='bash'
export EDITOR='vim'
export BOLD=`tput bold`
export UNDERLINE_ON=`tput smul`
export UNDERLINE_OFF=`tput rmul`
export TEXT_BLACK=`tput setaf 0`
export TEXT_RED=`tput setaf 1`
export TEXT_GREEN=`tput setaf 2`
export TEXT_YELLOW=`tput setaf 3`
export TEXT_BLUE=`tput setaf 4`
export TEXT_MAGENTA=`tput setaf 5`
export TEXT_CYAN=`tput setaf 6`
export TEXT_WHITE=`tput setaf 7`
export BACKGROUND_BLACK=`tput setab 0`
export BACKGROUND_RED=`tput setab 1`
export BACKGROUND_GREEN=`tput setab 2`
export BACKGROUND_YELLOW=`tput setab 3`
export BACKGROUND_BLUE=`tput setab 4`
export BACKGROUND_MAGENTA=`tput setab 5`
export BACKGROUND_CYAN=`tput setab 6`
export BACKGROUND_WHITE=`tput setab 7`
export RESET_FORMATTING=`tput sgr0`
export DB="(DESCRIPTION=(LOAD_BALANCE=on)(ADDRESS=(PROTOCOL=TCP)(HOST=10.36.1.201)(PORT=1521))(ADDRESS=(PROTOCOL=TCP)(HOST=10.36.1.202)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=EUDEV.EU.NAVTEQ.LOCAL)(SERVER=DEDICATED)))"
export TNS_ADMIN=/opt/instantclient
[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator
# Wrapper function for Maven's mvn command.
mvn-color()
{
  # Filter mvn output using sed
  M2_HOME=/opt/m2 /opt/m2/bin/mvn $@ | sed \
	       -e "s/\(\[INFO\]\ \-.*\)/${TEXT_BLUE}${BOLD}\1/g" \
               -e "s/\(\[INFO\]\ \[.*\)/${RESET_FORMATTING}${BOLD}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[INFO\]\ BUILD SUCCESSFUL\)/${BOLD}${TEXT_GREEN}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[WARNING\].*\)/${BOLD}${TEXT_YELLOW}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[ERROR\].*\)/${BOLD}${TEXT_RED}\1${RESET_FORMATTING}/g" \
               -e "s/Tests run: \([^,]*\), Failures: \([^,]*\), Errors: \([^,]*\), Skipped: \([^,]*\)/${BOLD}${TEXT_GREEN}Tests run: \1${RESET_FORMATTING}, Failures: ${BOLD}${TEXT_RED}\2${RESET_FORMATTING}, Errors: ${BOLD}${TEXT_RED}\3${RESET_FORMATTING}, Skipped: ${BOLD}${TEXT_YELLOW}\4${RESET_FORMATTING}/g"
 
  # Make sure formatting is reset
  echo -ne ${RESET_FORMATTING}
}

mvn()
{
  # Filter mvn output using sed
  /opt/maven/bin/mvn $@ | sed \
	       -e "s/\(\[INFO\]\ \-.*\)/${TEXT_BLUE}${BOLD}\1/g" \
               -e "s/\(\[INFO\]\ \[.*\)/${RESET_FORMATTING}${BOLD}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[INFO\]\ BUILD SUCCESSFUL\)/${BOLD}${TEXT_GREEN}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[WARNING\].*\)/${BOLD}${TEXT_YELLOW}\1${RESET_FORMATTING}/g" \
               -e "s/\(\[ERROR\].*\)/${BOLD}${TEXT_RED}\1${RESET_FORMATTING}/g" \
               -e "s/Tests run: \([^,]*\), Failures: \([^,]*\), Errors: \([^,]*\), Skipped: \([^,]*\)/${BOLD}${TEXT_GREEN}Tests run: \1${RESET_FORMATTING}, Failures: ${BOLD}${TEXT_RED}\2${RESET_FORMATTING}, Errors: ${BOLD}${TEXT_RED}\3${RESET_FORMATTING}, Skipped: ${BOLD}${TEXT_YELLOW}\4${RESET_FORMATTING}/g"
 
  # Make sure formatting is reset
  echo -ne ${RESET_FORMATTING}
}

jboss()
{
  sh run.sh -c $@ | sed \
		-e "s/\(\ INFO\)[ ]\+\(.*\)/${TEXT_WHITE}${BOLD}\1${RESET_FORMATTING}${TEXT_WHITE} \2/g" \
                -e "s/\(\ ERROR\)[ ]\+\(.*\)/${TEXT_YELLOW}${BOLD}\1${RESET_FORMATTING}${TEXT_YELLOW} \2/g" \
                -e "s/\(\ WARN\)[ ]\+\(.*\)/${TEXT_CYAN}${BOLD}\1${RESET_FORMATTING}${TEXT_CYAN} \2/g" \
	        -e "s/\(.*Exception.*\)/${TEXT_YELLOW}\1${RESET_FORMATTING}/g" \
		-e "s/\([	]at\ .*\)/${TEXT_YELLOW}\1${RESET_FORMATTING}/g" \
		-e "s/\(Started J2EE application\)/${TEXT_GREEN}${BOLD}\1${RESET_FORMATTING}/g" \
		-e "s/\(Started in [0-9]\+m:[0-9]\+s:[0-9]\+ms\)/${TEXT_GREEN}${BOLD}\1${RESET_FORMATTING}/g" \
		-e "s/\([0-9]\+:[0-9]\+:[0-9]\+\,[0-9]\+\)/${TEXT_BLUE} \1${RESET_FORMATTING}/g" \
		
}
 
# Override the mvn command with the colorized one.
#alias m2c="mvn-color"



c_cyan=`tput setaf 6`
c_red=`tput setaf 1`
c_green=`tput setaf 2`
c_sgr0=`tput sgr0`


#parse_git_branch() {
#  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\$(branch_color)git::\1\${c_sgr0})/'
#}
parse_svn_branch() {
  parse_svn_url | sed -e 's#^'"$(parse_svn_repository_root)"'##g' | awk -F / '{print "(svn::"$1 "/" $2 ")"}'
}
parse_svn_url() {
  svn info 2>/dev/null | grep -e '^URL*' | sed -e 's#^URL: *\(.*\)#\1#g '
}
parse_svn_repository_root() {
  svn info 2>/dev/null | grep -e '^Repository Root:*' | sed -e 's#^Repository Root: *\(.*\)#\1\/#g '
}
 
parse_git_branch ()
{
  if git rev-parse --git-dir >/dev/null 2>&1
  then
          gitver=$(git branch 2>/dev/null| sed -n '/^\*/s/^\* //p')
  else
          return 0
  fi
  echo -e '['$gitver'] '
}

branch_color ()
{
        if git rev-parse --git-dir >/dev/null 2>&1
        then
                 color=""
                if git diff --quiet 2>/dev/null >&2 
                then
                        color="${c_green}"
                else
                        color=${c_red}
                fi
        else
                return 0
        fi
        echo -ne $color
}
  
#PS1='\[$(branch_color)\]$(parse_git_branch)\[${c_sgr0}\]\u@\[${c_red}\]\w\[${c_sgr0}\]: '

# Check for an interactive session
[ -z "$PS1" ] && return
alias mvn='mvn'
alias mvnts='mvn -T 2.0C -Dmaven.test.skip'
#alias m2='M2_HOME=/opt/m2 /opt/m2/bin/mvn'
alias m2='mvn-color'
alias ls='ls --color=auto'
PS1='\[\033[00m\]\u@\h\[\033[01;34m\] \w \[$(branch_color)\]$(parse_git_branch)$(parse_svn_branch) \[\033[00m\]$\[\033[00m\] '
#PS1='[\u@\h \W]\$ '
#PS1='[\u@\h ${BOLD}${TEXT_GREEN}`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`\[\033[37m\]\[\033[00m\]\W]\$ '
export MAVEN_OPTS="-Xmx1024m -XX:MaxPermSize=512m"
