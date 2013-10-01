#!/bin/bash
set -xe

################################
#Install emacs, screen, and git
sudo apt-get update
sudo apt-get install -yy --no-install-recommends emacs screen git-core


################################
#allow password-based ssh logins
#(assume we're on a throwaway cloud machine)
TMPFILE=/tmp/new_sshd_config
cat <<EOF > $TMPFILE
Port 22
Protocol 2
HostKey /etc/ssh/ssh_host_rsa_key
HostKey /etc/ssh/ssh_host_dsa_key
HostKey /etc/ssh/ssh_host_ecdsa_key
SyslogFacility AUTH
PermitRootLogin no
StrictModes yes
RSAAuthentication yes
PubkeyAuthentication yes
PasswordAuthentication yes
PermitEmptyPasswords no
X11Forwarding no
PrintMotd no
PrintLastLog yes
TCPKeepAlive yes
AcceptEnv LANG LC_*
EOF

sudo mv $TMPFILE /etc/ssh/sshd_config
sudo reload ssh
