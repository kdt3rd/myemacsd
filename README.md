Customizing emacs
=================

This should be cloned as

git clone --recurse-submodules https://github.com/kdt3rd/myemacsd.git .emacs.d

if you forget the --recurse-submodules just do
git submodule init
git submodule update

if you are just pulling this repo, run both of these to update to the latest

git pull
git submodule update --init --recursive

Submodules
----------

These should be monitored to see if they are in elpa at some point (or if we don't
want to use elpa, then more need to be added here)

additionally, if you want a particular branch or something you can do something like
git config -f .gitmodules submodule.Foobar.branch stable

a handy trick if it doesn't remember for you

git config status.submodulesummary 1

will then show the submodule statii during git status and git diff as well as being able to do
git log -p --submodule

To update submodules locally:
git submodule update --remote

git submodule add https://github.com/manateelazycat/color-rg.git site-elisp/color-rg
git submodule add https://github.com/immerrr/lua-mode.git site-elisp/lua-mode
git submodule add https://github.com/yoshiki/yaml-mode.git site-elisp/yaml-mode
