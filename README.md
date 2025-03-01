# cobweb

![COBOL](https://img.shields.io/badge/COBOL-0000FF?style=flat&logo=COBOL&logoColor=white)

Welcome to **CobWeb**! 🎉

Honestly, I did it on a dare. CobWeb is a simple webserver, written in COBOL, that can serve static websites. It serves the most common MIME types and can handle basic HTTP GET requests. Minimal further improvement can be expected, as the next step is implementing a correct logging system for the webserver.

The program does not have any explicit purpose other than to prove that such program, a webserver written in COBOL exists.

The webserver, build, makefile was tested only on Debian 12 (bookworm) and GNUCobol 3.1.2.0

The minimal configuration file for the server can be edited under /etc/cobweb/cobweb.conf

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
- Copy the config file to `/etc/cobweb`
- Add the systemd service file to `/etc/systemd/system`

### Uninstall It

Run this to uninstall the web server:

```sh
sudo make uninstall
```

This will:
- Stop the cobweb service
- Disable the cobweb service
- Remove the binary from `/usr/local/bin`
- Delete the config file from `/etc/cobweb`
- Remove the systemd service file from `/etc/systemd/system`
- Reload systemd


### Systemd service


```sh
make run
```
This will:
- Install the systemd service
- Enables the systemd service
- Starts the systemd service

### Clean Up

Run this to clean up the build files:

```sh
make clean
```

## Configuration

The config file is in `config/cobweb.conf`. Feel free to tweak it to suit your needs.

## Systemd Service

The service file is in `config/cobweb.service`. This file tells systemd how to manage your web server.

## Why COBOL?

Why not?

## License

This project is licensed under the MIT License.
