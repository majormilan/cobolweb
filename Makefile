# Variables
COBOL_SRC = sources/webserver.cbl
COBOL_EXEC = build/cobolweb
INSTALL_PATH = /usr/local/bin
CONFIG_DIR = /etc/cobolweb
CONFIG_FILE = config/cobolweb.conf
SYSTEMD_DIR = /etc/systemd/system
SERVICE_FILE = config/cobolweb.service

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
	@echo "Installing COBOL web server..."
	install -d $(INSTALL_PATH)
	install -m 755 $(COBOL_EXEC) $(INSTALL_PATH)
	install -d $(CONFIG_DIR)
	install -m 644 $(CONFIG_FILE) $(CONFIG_DIR)

# Run the systemd service
run: install
	install -m 644 $(SERVICE_FILE) $(SYSTEMD_DIR)
	@echo "Starting COBOL web server..."
	systemctl daemon-reload
	systemctl enable cobolweb
	systemctl start cobolweb

# Uninstall the binary, config file, and systemd service
uninstall:
	@echo "Uninstalling COBOL web server..."
	systemctl stop cobolweb
	systemctl disable cobolweb
	rm -f $(INSTALL_PATH)/cobolweb
	rm -f $(CONFIG_DIR)/cobolweb.conf
	rm -f $(SYSTEMD_DIR)/cobolweb.service
	systemctl daemon-reload

# Clean up build files
clean:
	@echo "Cleaning up..."
	rm -f $(COBOL_EXEC)
