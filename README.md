
# Table of Contents

1.  [Installation](#org1af73e8)
    1.  [Platforms supported](#orgb6177ea)
    2.  [Emacs](#org6bd02dc)
        1.  [Arch-linux](#orgee92bc4)
        2.  [Post installation](#orgdcec9b3)
2.  [Inspiration](#org8a21975)

The Nerdy Hamster Emacs, is my personal emacs configuration. That I am customizing after my needs.


<a id="org1af73e8"></a>

# Installation


<a id="orgb6177ea"></a>

## Platforms supported

-   [X] Linux
-   [ ] Windows
-   [ ] MacOS

This Emacs configuration is currently only tested on `Arch Linux`.


<a id="org6bd02dc"></a>

## Emacs

> I am currently running Emacs `28.0.50` `Master` Branch, the repo is **only** tested on Version `28.0.50`.


<a id="orgee92bc4"></a>

### Arch-linux

**Recomended** Install Emacs `28.0.50` or above.

    yay -S emacs-git


<a id="orgdcec9b3"></a>

### Post installation

Clone this repository to the `.emacs.d/` directory.

    git clone https://github.com/TheNerdyHamster/The-Nerdy-Hamster-Emacs.git ~/.emacs.d/

1.  Icons

    Packages such as `Centaur-tabs`, `Treemacs`, `doom-modeline` utilizes features from `all-the-icons`.
    
    When you start up emacs for the first time, run the following command.
    
        M-x (all-the-icons-install-font)


<a id="org8a21975"></a>

# Inspiration

[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) provides a good list of useful packages.

Other emacs/dotfiles repos for inspiration

-   [Daviwil's dotfiles](https://github.com/daviwil/dotfiles)
-   [Daviwil's emacs from scratch repo](https://github.com/daviwil/emacs-from-scratch)
-   [Angrybacon's dotemacs](https://github.com/angrybacon/dotemacs)

