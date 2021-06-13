
# Table of Contents

1.  [Installation](#orgdb85957)
    1.  [Platforms supported](#org526bb74)
    2.  [Emacs](#org03c5bec)
        1.  [Arch-linux](#org379f144)
        2.  [Post installation](#org39c09d3)
2.  [Inspiration](#org127b165)

The Nerdy Hamster Emacs, is my personal emacs configuration. That I am customizing after my needs.


<a id="orgdb85957"></a>

# Installation


<a id="org526bb74"></a>

## Platforms supported

[ X ] Linux
[ ] Windows 
[ ] MacOS

This Emacs configuration is currently only tested on `Arch Linux`.


<a id="org03c5bec"></a>

## Emacs

> I am currently running Emacs `28.0.50` `Master` Branch, the repo is **only** tested on Version `28.0.50`.


<a id="org379f144"></a>

### Arch-linux

**Recomended** Install Emacs `28.0.50` or above.

    yay -S emacs-git


<a id="org39c09d3"></a>

### Post installation

Clone this repository to the `.emacs.d/` directory.

    git clone https://github.com/TheNerdyHamster/The-Nerdy-Hamster-Emacs.git ~/.emacs.d/

1.  Icons

    Packages such as `Centaur-tabs`, `Treemacs`, `doom-modeline` utilizes features from `all-the-icons`.
    
    When you start up emacs for the first time, run the following command.
    
        M-x (all-the-icons-install-font)


<a id="org127b165"></a>

# Inspiration

[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) provides a good list of useful packages.

Other emacs/dotfiles repos for inspiration

-   [Daviwil's dotfiles](https://github.com/daviwil/dotfiles)
-   [Daviwil's emacs from scratch repo](https://github.com/daviwil/emacs-from-scratch)
-   [Angrybacon's dotemacs](https://github.com/angrybacon/dotemacs)

