typedef unsigned long time_t;
typedef long step_t;

class Motor {
public:
  step_t targetPos, currentPos;
  unsigned long rate; // microseconds / step
private:
  time_t lastStep; // microseconds
  int enPin, dirPin, stepPin;
  boolean enabled;

private:
  void takeStep(int dir) {
    if (dir > 0) {
      digitalWrite(dirPin, 1);
      currentPos += 1;
    } else {
      digitalWrite(dirPin, 0);      
      currentPos -= 1;
    }
    
    digitalWrite(stepPin, 1);
    digitalWrite(stepPin, 0);
  }
  
public:
  Motor(int enPin, int dirPin, int stepPin)
    : targetPos(0), currentPos(0), rate(100)
    , lastStep(0)
    , enPin(enPin), dirPin(dirPin), stepPin(stepPin)
    , enabled(false)
  {
    pinMode(enPin, OUTPUT);
    pinMode(dirPin, OUTPUT);
    pinMode(stepPin, OUTPUT);
    setEnable(false);
  }
  
  void setEnable(boolean on) {
    digitalWrite(enPin, !on);
    enabled = on;
  }
  
  void iterate() {
    unsigned long t = micros();
    if (targetPos != currentPos && abs(t - lastStep) > rate) {
      if (!enabled) setEnable(true);
      takeStep(targetPos > currentPos ? +1 : -1);
      lastStep = t;
    }
    if (targetPos == currentPos && enabled && t - lastStep > 100000)
      setEnable(false);
  }
  
  void info() {
    Serial.print("EN pin    "); Serial.println(enPin);
    Serial.print("DIR pin   "); Serial.println(dirPin);
    Serial.print("STEP pin  "); Serial.println(stepPin);
  }
};

const int nMotors = 1;
Motor motors[nMotors] = {
  Motor(5, 7, 6)
};

void setup() {
  Serial.begin(115200);
}

double rToTemperature(double beta, double t0, double r0, double r) {
  return 1 / (1/t0 + 1/beta*log(r/r0));
}

void handleTempCommand(int channel) {
  float x = analogRead(channel);
  float r = 1e4*(1023 / x - 1);
  double temp = rToTemperature(3470, 25+273, 1e4, r);
  Serial.print("Temperature (C)   "); Serial.println(temp-273);
}

char cmd[255];
int head = 0;
unsigned int currentMotor = 0;

void handleCommand() {
  Motor& m = motors[currentMotor];
  if (strncmp("select", cmd, 6) == 0) {
    char *a = cmd, *tmp;
    while (isalpha(*a) || isblank(*a)) a++;
    unsigned long motor = strtoul(a, &tmp, 10);
    if (tmp == a)
      Serial.println("!ERR Invalid number");
    else if (motor >= nMotors)
      Serial.println("!ERR Invalid motor number");
    else {
      currentMotor = motor;
      Serial.println("OK");
    } 
  } else if (strncmp("rate", cmd, 4) == 0) {
    char *a = cmd, *tmp;
    while (isalpha(*a) || isblank(*a)) a++;
    unsigned long rate = strtoul(a, &tmp, 10);
    if (tmp == a)
      Serial.println("!ERR Invalid number");
    else {
      m.rate = rate;
      Serial.println("OK");
    }
  } else if (strncmp("rel move", cmd, 8) == 0) {
    char *a = cmd, *tmp;
    while (isalpha(*a) || isblank(*a)) a++;
    long pos = strtol(a, &tmp, 10);
    if (tmp == a)
      Serial.println("!ERR Invalid number");
    else {
      m.targetPos += pos;
      Serial.println("OK");
    }
  } else if (strncmp("move", cmd, 4) == 0) {
    char *a = cmd, *tmp;
    while (isalpha(*a) || isblank(*a)) a++;
    long pos = strtol(a, &tmp, 10);
    if (tmp == a)
      Serial.println("!ERR Invalid number");
    else {
      m.targetPos = pos;
      Serial.println("OK");
    }
  } else if (strncmp("zero", cmd, 4) == 0) {
    m.targetPos = 0;
    Serial.println("OK");
  } else if (strncmp("status", cmd, 6) == 0) {
    Serial.print("Motor ");     Serial.println(currentMotor);
    Serial.print("rate      "); Serial.println(m.rate);
    Serial.print("target    "); Serial.println(m.targetPos);
    Serial.print("position  "); Serial.println(m.currentPos);
    Serial.println("OK");
  } else if (strncmp("temp 0", cmd, 6) == 0) {
    handleTempCommand(0);
  } else if (strncmp("temp 1", cmd, 6) == 0) {
    handleTempCommand(1);
  } else if (strncmp("info", cmd, 4) == 0) {
    Serial.print("Motor ");
    Serial.println(currentMotor);
    m.info();
    Serial.println("OK");
  }
}

void handleInput() {
  while (Serial.available() > 0) {
    cmd[head] = Serial.read();
    Serial.write(cmd[head]);
    if (cmd[head] == '\n') {
      cmd[head+1] = '\0';
      handleCommand();
      head = 0;
    } else if (cmd[head] != -1)
      head = (head+1) % 256;
  }
}

void loop() {
  handleInput(); 
  for (int i=0; i<nMotors; i++)
    motors[i].iterate();
}

