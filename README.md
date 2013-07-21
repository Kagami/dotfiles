# Dotfiles the easy way

Why the hell everyone come in with tons of useless kludgy solutions for such simple task? Just USE Git!

## Usage

is very simple.

```sh
% git clone --bare https://github.com/Kagami/dotfiles.git ~/code/dotfiles.git
% alias gdot='git --git-dir=$HOME/code/dotfiles.git --work-tree=$HOME'
# Don't forget to backup your current dotfiles!!!
% gdot reset --hard
% gdot submodule init
% gdot submodule update
```

That's it. Use `gdot` as `git` for future manipulations with dotfiles and Git will do it just perfectly.

Note that `.gitignore` contains `*` so you will need to use `gdot add -f` for adding new files. It guarantee that you won't accidentally add private data to the repo.
