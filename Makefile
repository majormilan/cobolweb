# Variables
COBOL_SRC = webserver.cbl
COBOL_EXEC = cobolweb
INSTALL_PATH = /usr/local/bin
CONFIG_DIR = /etc/cobolweb
CONFIG_FILE = cobolweb.conf
SYSTEMD_DIR = /etc/systemd/system
SERVICE_FILE = cobolweb.service

.PHONY: all clean install uninstall

# Default target
all: $(COBOL_EXEC)

# Compile COBOL program
$(COBOL_EXEC): $(COBOL_SRC)
	@echo "Compiling COBOL source..."
	cobc -x -o $(COBOL_EXEC) $(COBOL_SRC)

# Install the binary, config file, and systemd service
install: $(COBOL_EXEC)
	@echo "Installing COBOL web server..."
	install -d $(INSTALL_PATH)
	install -m 755 $(COBOL_EXEC) $(INSTALL_PATH)
	install -d $(CONFIG_DIR)
	install -m 644 $(CONFIG_FILE) $(CONFIG_DIR)
	install -m 644 $(SERVICE_FILE) $(SYSTEMD_DIR)
	systemctl daemon-reload
	systemctl enable cobolweb
	systemctl start cobolweb

# Uninstall the binary, config file, and systemd service
uninstall:
	@echo "Uninstalling COBOL web server..."
	systemctl stop cobolweb
	systemctl disable cobolweb
	rm -f $(INSTALL_PATH)/$(COBOL_EXEC)
	rm -f $(CONFIG_DIR)/$(CONFIG_FILE)
	rm -f $(SYSTEMD_DIR)/$(SERVICE_FILE)
	systemctl daemon-reload

# Clean up build files
clean:
	@echo "Cleaning up..."
	rm -f $(COBOL_EXEC)
