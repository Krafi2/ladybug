FROM opensuse/tumbleweed
RUN zypper --non-interactive install shadow sudo flatpak flatpak-remote-flathub rustup
RUN zypper refresh; flatpak update --appstream # Refresh the package managers so that we won't have to wait later
RUN echo "ALL ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN useradd -ms /bin/bash test
USER test
RUN flatpak update --appstream # This is required to force flatpak to use the correct user
RUN rustup toolchain install stable
COPY --chown=test ./ /home/test/ladybug
WORKDIR /home/test/ladybug
RUN mv dotfiles ~/ && mkdir ~/.config/ladybug/ && mv config.ldbg ~/.config/ladybug/
RUN CARGO_UNSTABLE_SPARSE_REGISTRY=true cargo build && sudo ln -s /home/test/ladybug/target/debug/ladybug /usr/bin
WORKDIR /home/test
ENTRYPOINT bash -l