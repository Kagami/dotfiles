# Dotfiles the easy way

Why the hell everyone come in with tons of useless kludgy solutions for such simple task? Just USE Git!

## Usage

is very simple.

```sh
% mkdir -p ~/code/
% git clone --bare https://github.com/Kagami/dotfiles.git ~/code/dotfiles.git
% alias gdot='git --git-dir=$HOME/code/dotfiles.git --work-tree=$HOME'
# Don't forget to backup your current dotfiles!
% gdot reset --hard
% gdot submodule init
% gdot submodule update
```

That's it. Use `gdot` as `git` for future manipulations with dot files and git will do it just perfectly.

Note that `.gitignore` contain `*` so you will need to use `gdot add -f` for addint new files. It garantee what you won't accidentally add some private data to the repository.
