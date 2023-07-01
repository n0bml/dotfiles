#!/bin/bash

# Change Debian to SID Branch
cp /etc/apt/sources.list /etc/apt/sources.list.dist
cp sources.list /etc/apt/sources.list 


username=$(id -u -n 1000)
builddir=$(pwd)


# Update packages list
apt update
apt upgrade
apt autoremove

# Add base packages
apt install unzip git curl tree build-essential

cd $builddir
mkdir -p /home/$username/.config
chown -R $username:$username /home/$username

# ms fonts
sudo apt install ttf-mscorefonts-installer
