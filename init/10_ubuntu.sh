# Ubuntu/Debian only stuff
[[ "$(cat /etc/issue 2> /dev/null)" =~ Ubuntu ]] || return 1

# Update APT.
e_header "Updating APT"
sudo apt-get -qq update

# Install APT packages.
packages=(
    git-core
    tree
    htop
)

list=()
for package in "${packages[@]}"; do
    if [[ ! "$(dpkg -l "$package" 2>/dev/null | grep "^ii  $package")" ]]; then
        list=("${list[@]}" "$package")
    fi
done

if (( ${#list[@]} > 0 )); then
    e_header "Installing APT packages: ${list[*]}"
    for package in "${list[@]}"; do
        sudo apt-get -qq install "$package"
    done
fi
