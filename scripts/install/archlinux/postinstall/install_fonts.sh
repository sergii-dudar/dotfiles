wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/JetBrainsMono.zip \
&& cd ~/.local/share/fonts \
&& unzip JetBrainsMono.zip \
&& rm JetBrainsMono.zip \
&& fc-cache -fv

wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/Hack.zip \
&& cd ~/.local/share/fonts \
&& unzip Hack.zip \
&& rm Hack.zip \
&& fc-cache -fv

wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/CascadiaCode.zip \
&& cd ~/.local/share/fonts \
&& unzip CascadiaCode.zip \
&& rm CascadiaCode.zip \
&& fc-cache -fv

# brew install --cask font-hack-nerd-font
# brew install --cask font-jetbrains-mono-nerd-font
# brew install --cask font-cascadia-code-nf