# cobolweb

![COBOL](https://img.shields.io/badge/COBOL-0000FF?style=flat&logo=COBOL&logoColor=white)

Welcome to **cobolweb**! 🎉

cobolweb is a proof of concept program designed to show that COBOL can handle sockets, ports, and web services. Yep, you heard that right – we're using COBOL to run a web server! 🚀

## What You Need

- GNUCobol
- make utility
- systemd (optional, but the Makefile assumes that you run systemd)

## Getting Started

### Build It

Run this to compile the COBOL source code:

```sh
make
```
The binary will be in the build directory.

### Install It

Run this to install the web server:

```sh
sudo make install
```

This will:
- Put the binary in `/usr/local/bin`
- Copy the config file to `/etc/cobolweb`
- Add the systemd service file to `/etc/systemd/system`
- Reload systemd
- Enable and start the cobolweb service

### Uninstall It

Run this to uninstall the web server:

```sh
sudo make uninstall
```

This will:
- Stop the cobolweb service
- Disable the cobolweb service
- Remove the binary from `/usr/local/bin`
- Delete the config file from `/etc/cobolweb`
- Remove the systemd service file from `/etc/systemd/system`
- Reload systemd

### Clean Up

Run this to clean up the build files:

```sh
make clean
```

## Configuration

The config file is in `config/cobolweb.conf`. Feel free to tweak it to suit your needs.

## Systemd Service

The service file is in `config/cobolweb.service`. This file tells systemd how to manage your web server.

## Why COBOL?

Why not?

## License

This project is licensed under the MIT License.
