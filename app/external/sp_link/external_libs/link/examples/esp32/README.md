# *E X P E R I M E N T A L* 

*Tested with esp-idf [v4.3.1](https://github.com/espressif/esp-idf/releases/tag/v4.3.1)*

## Building and Running the Example

* Setup esp-idf as described in [the documentation](https://docs.espressif.com/projects/esp-idf/en/latest/get-started/index.html)
* Run `idf.py menuconfig` and setup WiFi credentials under 
`Example Connection Configuration`
```
idf.py build
idf.py -p ${ESP32_SERIAL_PORT} flash
