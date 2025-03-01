# Variables
COBOL_SRC = sources/webserver.cbl
COBOL_EXEC = build/cobweb
INSTALL_PATH = /usr/local/bin
CONFIG_DIR = /etc/cobweb
CONFIG_FILE = config/cobweb.conf
SYSTEMD_DIR = /etc/systemd/system
SERVICE_FILE = config/cobweb.service

.PHONY: all clean install uninstall run

# Default target
all: check-root $(COBOL_EXEC)

# Ensure the script is run as root
check-root:
	@if [ "$$(id -u)" -ne 0 ]; then echo "This script must be run as root"; exit 1; fi
# Create build directory if it doesn't exist
build:
	@mkdir -p build

# Compile COBOL program
$(COBOL_EXEC): build $(COBOL_SRC)
	@echo "Compiling COBOL source..."
	cobc -x -o $(COBOL_EXEC) $(COBOL_SRC)

# Install the binary, config file, and systemd service
install: $(COBOL_EXEC)
	@echo "Installing cobweb server..."
	install -d $(INSTALL_PATH)
	install -m 755 $(COBOL_EXEC) $(INSTALL_PATH)
	install -d $(CONFIG_DIR)
	install -m 644 $(CONFIG_FILE) $(CONFIG_DIR)

# Run the systemd service
run: install
	install -m 644 $(SERVICE_FILE) $(SYSTEMD_DIR)
	@echo "Starting cobweb server..."
	systemctl daemon-reload
	systemctl enable cobweb
	systemctl start cobweb

# Uninstall the binary, config file, and systemd service
uninstall:
	@echo "Uninstalling cobweb server..."
	systemctl stop cobweb
	systemctl disable cobweb
	rm -f $(INSTALL_PATH)/cobweb
	rm -f $(CONFIG_DIR)/cobweb.conf
	rm -f $(SYSTEMD_DIR)/cobweb.service
	systemctl daemon-reload

# Clean up build files
clean:
	@echo "Cleaning up..."
	rm -f $(COBOL_EXEC)
