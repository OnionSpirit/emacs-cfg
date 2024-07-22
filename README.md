## Installation  
- **Install ```emacs``` to your linux distro if needed**
- **Remove ```.emacs.d``` if it is exists and clone config**
```
rm -rf ~/.emacs.d
git clone --depth 1 https://github.com/OnionSpirit/emacs-cfg.git ~/.emacs.d
```
> If developer 
> ```
> rm -rf ~/.emacs.d
> git clone --depth 1 git@github.com:OnionSpirit/emacs-cfg.git ~/.emacs.d
> ```

- **Launch deploy script**
```
~/.emacs.d/deploy.sh
```
- **Run Emacs**
> **For GUI mode** 
> ```
> emacs
>```
> **For terminal mode** 
> ```
> macs
> ```
- **Wait until dashboard shown,
 that means all packages are installed,
 up to date and Emacs are ready to work**
- **Enjoy**
---


## Extra  
- **```macs``` alias will be provided for ```zsh``` and ```bash``` shells after installation for terminal mode launch**
---
