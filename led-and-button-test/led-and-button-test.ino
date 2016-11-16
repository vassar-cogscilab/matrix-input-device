int current_column = 0;

static int LED_buffer[8][8];
static int debounce_count[8][8];
static const int MAX_DEBOUNCE = 2;

static const int button_col_pins[8] = {2, 3, 4, 5, 6, 7, 8, 9};
static const int button_row_pins[8] = {29, 28, 27, 26, 25, 24, 23, 22};
static const int row_pins[8][3] = {{29,37,49}, {28,36,48}, {27,35,47}, {26,34,46}, {25,33,45}, {24,32,44}, {23,31,43}, {22,30,42}};
static const int col_pins[8] = {53, 52, 51, 50, 41, 40, 39, 38};

void setup() {
 // Serial.begin(9600);
  
  // LEDs
  for(int i=0; i<8; i++){
    for(int j=0; j<3; j++){
      pinMode(row_pins[i][j], OUTPUT);
      digitalWrite(row_pins[i][j], HIGH);
    }
    pinMode(col_pins[i], OUTPUT);
    digitalWrite(col_pins[i], LOW);
  }
  // Buttons
  for(int i=0; i<8; i++){
    pinMode(button_col_pins[i], INPUT);
  }
  
  // LED state buffer
  for(int i=0; i<8; i++){
    for(int j=0; j<8; j++){
      LED_buffer[i][j] = 0;
      debounce_count[i][j] = 0;
    }
  }
  
  //Serial.println("Setup complete");
}

void LEDscan(int current_column) {
  int buffer_val;
  //Serial.println('LEDScan');
  // Select LED column
  digitalWrite(col_pins[current_column], HIGH);
  
  // Illuminate LEDs
  for(int row = 0; row < 8; row++){
    buffer_val = LED_buffer[row][current_column]; // 0 = not lit, 1 = Red, 2 = Green, 3 = Blue
    if(buffer_val > 0){
      digitalWrite(row_pins[row][buffer_val - 1], LOW);
      delay(1);
      digitalWrite(row_pins[row][buffer_val - 1], HIGH);
    }
  }
  
  
  //Serial.println("scan loop");
  
  // Deselect LED column
  digitalWrite(col_pins[current_column], LOW);
  
}

void buttonScan(int current_row){
  int val;
  
  // Select BUTTON row
  digitalWrite(button_row_pins[current_row], LOW);
  
  // Read buttons
  for(int col = 0; col < 8; col++){
    val = digitalRead(button_col_pins[col]);
    //Serial.print(val);
    if(val == HIGH){
      if(debounce_count[current_row][col] < MAX_DEBOUNCE){
        debounce_count[current_row][col]++;
        if(debounce_count[current_row][col] == MAX_DEBOUNCE){
          LED_buffer[current_row][col]++;
          if(LED_buffer[current_row][col] >= 4){
            LED_buffer[current_row][col] = 0;
          }
          /*Serial.println("Button pressed:");
          Serial.print(current_row);
          Serial.print("\t");
          Serial.print(col);
          Serial.println();*/
        }
      }
    } else {
      if(debounce_count[current_row][col] > 0){
        debounce_count[current_row][col]--;
        if(debounce_count[current_row][col] == 0){
          // button is released
        }
      }
    }
  }
  
  // RESET button column
  digitalWrite(button_row_pins[current_row], HIGH);
}

void scan() {
  
  LEDscan(current_column);
  buttonScan(current_column);
  
  current_column++;
  if(current_column == 8) {
    current_column = 0;
  }
}

void loop() {
  
  scan();
  
}
