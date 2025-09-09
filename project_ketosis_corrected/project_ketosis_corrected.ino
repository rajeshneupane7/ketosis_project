#include <Adafruit_BME280.h>
#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <MQUnifiedsensor.h>
#define MQ_PIN A0
#define RL_VALUE 10.0
#define RO_CLEAN_AIR_FACTOR 3.08
#define OLED_RESET -1 // Define the OLED reset pin (use -1 if not connected)
#define placa "Arduino UNO"
#define Voltage_Resolution 5
#define pin A0 // Analog input 0 of your Arduino
#define type "MQ-135" // MQ135
#define ADC_Bit_Resolution 10 // For Arduino UNO/MEGA/NANO
#define RatioMQ135CleanAir 3.6 // RS / R0 = 3.6 pp
#define CORA 0.00035
#define CORB 0.02718
#define CORC 1.39538
#define CORD 0.0018
Adafruit_BME280 bme; // use I2C interface
Adafruit_Sensor *bme_temp = bme.getTemperatureSensor();
Adafruit_Sensor *bme_pressure = bme.getPressureSensor();
Adafruit_Sensor *bme_humidity = bme.getHumiditySensor();
#if defined(ESP8266)
  #define BUTTON_A  0
  #define BUTTON_B 16
  #define BUTTON_C  2
  #define WIRE Wire
#elif defined(ARDUINO_ADAFRUIT_FEATHER_ESP32C6)
  #define BUTTON_A  7
  #define BUTTON_B  6
  #define BUTTON_C  5
  #define WIRE Wire
#elif defined(ESP32)
  #define BUTTON_A 15
  #define BUTTON_B 32
  #define BUTTON_C 14
  #define WIRE Wire
#elif defined(ARDUINO_STM32_FEATHER)
  #define BUTTON_A PA15
  #define BUTTON_B PC7
  #define BUTTON_C PC5
  #define WIRE Wire
#elif defined(TEENSYDUINO)
  #define BUTTON_A  4
  #define BUTTON_B  3
  #define BUTTON_C  8
  #define WIRE Wire
#elif defined(ARDUINO_FEATHER52832)
  #define BUTTON_A 31
  #define BUTTON_B 30
  #define BUTTON_C 27
  #define WIRE Wire
#elif defined(ARDUINO_ADAFRUIT_FEATHER_RP2040)
  #define BUTTON_A  9
  #define BUTTON_B  8
  #define BUTTON_C  7
  #define WIRE Wire
#else // 32u4, M0, M4, nrf52840 and 328p
  #define BUTTON_A  9
  #define BUTTON_B  6
  #define BUTTON_C  5
  #define WIRE Wire
#endif
Adafruit_SSD1306 display = Adafruit_SSD1306(128, 32, &WIRE);
MQUnifiedsensor MQ135(placa, Voltage_Resolution, ADC_Bit_Resolution, pin, type);
float readMQ();
float getResistance(float sensor_volt);
float getCorrectionFactor(float t, float h);


void setup() {
  Serial.begin(9600);
  
  Serial.println("OLED FeatherWing test");
  // SSD1306_SWITCHCAPVCC = generate display voltage from 3.3V internally
  display.begin(SSD1306_SWITCHCAPVCC, 0x3C); // Address 0x3C for 128x32

  Serial.println("OLED begun");

  // Show image buffer on the display hardware.
  // Since the buffer is intialized with an Adafruit splashscreen
  // internally, this will display the splashscreen.
  display.display();
  delay(1000);

  // Clear the buffer.
  display.clearDisplay();
  display.display();

  Serial.println("IO test");

  pinMode(BUTTON_A, INPUT_PULLUP);
  pinMode(BUTTON_B, INPUT_PULLUP);
  pinMode(BUTTON_C, INPUT_PULLUP);

  // text display tests
  display.setTextSize(1);
  display.setTextColor(SSD1306_WHITE);
  display.setCursor(0,0);
  display.print("Connecting to SSID\n'adafruit':");
  display.print("connected!");
  display.println("IP: 10.0.1.23");
  display.println("Sending val #0");
  display.setCursor(0,0);
  display.display();
  
  
  display.clearDisplay();
  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(0, 0);
  display.println("Initializing");
  display.println("Cow Acetone Breathalyzer...");
  display.display();
  delay(2000);
  MQ135.init();

  // Calibration
  Serial.print("Calibrating please wait.");
  float calcR0 = 0;
  for (int i = 1; i <= 10; i++) {
    MQ135.update();
    calcR0 += MQ135.calibrate(RatioMQ135CleanAir);
    Serial.print(".");
  }
  MQ135.setR0(calcR0 / 10);
  Serial.println(" done!");

  if (isinf(calcR0)) {
    Serial.println("Warning: Connection issue, R0 is infinite (Open circuit detected) please check your wiring and supply");
    while (1);
  }
  if (calcR0 == 0) {
    Serial.println("Warning: Connection issue found, R0 is zero (Analog pin shorts to ground) please check your wiring and supply");
    while (1);
  }

  Serial.println("Acetone Measurement Initialized");

    if (!bme.begin()) {
    Serial.println(F("Could not find a valid BME280 sensor, check wiring!"));
    while (1) delay(10);
  }
  
  bme_temp->printSensorDetails();
  bme_pressure->printSensorDetails();
  bme_humidity->printSensorDetails();
}

void loop() {
  // Read the sensor voltage
  sensors_event_t temp_event, pressure_event, humidity_event;
  bme_temp->getEvent(&temp_event);
  bme_pressure->getEvent(&pressure_event);
  bme_humidity->getEvent(&humidity_event);

  float sensor_volt = analogRead(MQ_PIN) / 1024.0 * 5.0;

  // Calculate resistance
  float rs = getResistance(sensor_volt);

  // Calculate ratio and ppm
  float ratio = rs / RO_CLEAN_AIR_FACTOR;
  float ppm = pow(ratio, -1.179) * 4.385;
  float mgL = ppm * 1.995; // Convert PPM to mg/L

  MQ135.update(); // Update data from the sensor

  // Configure the equation to calculate Acetone concentration value
  MQ135.setA(34.668); // Acetone coefficient a
  MQ135.setB(-3.369); // Acetone coefficient b
  float cFactor = getCorrectionFactor(temp_event.temperature,temp_event.relative_humidity);
  float Acetone = MQ135.readSensor(false, cFactor); // Read Acetone concentration
  // float Acetone = MQ135.readSensor();
  // Display the result
  Serial.print("Acetone Concentration (PPM): ");
  Serial.println(Acetone);

  delay(5000); // Sampling frequency
  // Display the ppm value on the OLED
  display.clearDisplay(); 
  
  // display.setCursor(0, 0); // Set the cursor to the top of the display
  // display.println("Dairy Lab"); // Line of dashes
  // display.println("Reading Every 5 S"); // New reading message // Move cursor down for reading data
  // display.println("Acetone PPM: " );
  // display.print(Acetone, 2); // Display PPM value with 2 decimal places
  // display.display();
  // Clear the display before printing new data
display.clearDisplay(); 

// Set cursor to the top of the display for Acetone
display.setCursor(0, 0); 

// Print "Acetone PPM:" and its value on the same line
display.print("Acetone PPM: ");
display.print(Acetone, 2); // Display with 2 decimal places

// Move cursor to the next line for Humidity
display.setCursor(0, 10); // Adjust based on screen size (Y-coordinate 10 pixels down)
display.print("Humidity: ");
display.print(humidity_event.relative_humidity);
display.println("%"); // Add percentage symbol

// Move cursor to the next line for Temperature
display.setCursor(0, 20); // Further down for temperature
display.print("Temp: ");
display.print(temp_event.temperature);
display.println(" C"); // Add degree symbol and "C" for Celsius

// Render the updated content on the OLED
display.display();


  // Wait for 5 seconds before reading again
  delay(5000);

}

float getResistance(float sensor_volt) {
  return RL_VALUE * (5.0 - sensor_volt) / sensor_volt;
}

float getCorrectionFactor(float t, float h) {
  return CORA * t * t - CORB * t + CORC - (h-33.)*CORD;


}

