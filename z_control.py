#!/usr/bin/python

import evinput
from time import time, sleep
import serial
import logging
from numpy import copysign, abs

def find_device():
        from glob import glob
        known_devices = [
                (0x046d, 0xC626) # 3Dconnexion Space Navigator
        ]
        for dev in glob('/dev/input/event*'):
                e = evinput.Device(dev)
                devid = e.read_device_id()
                logging.info("Found device %s (%x:%x)" % (dev, devid.vendor, devid.product))
                if (devid.vendor, devid.product) in known_devices:
                        return e

import threading

        

if __name__ == '__main__':
        s = serial.Serial('/dev/ttyACM0', baudrate=115200)
        s.write('rate 1000\n')

        invert = True
        gain = 5e-0
        velocity = 0
        stopped = True
        def update_loop():
                global velocity, stop
                last_update = time()
                while not stopped:
                        t = time()
                        dt = t - last_update
                        dx = round(dt * velocity)
                        if dx == 0: continue
                        if dx != 0: print velocity
                        s.write('rel move %d\n' % dx)
                        s.readline()
                        s.readline()
                        last_update = t
                        sleep(5e-2)

        dev = find_device()
        while True:
                e = dev.read_event()
                if e.evtype == evinput.evtypes.REL and e.code == 2:
                        if abs(e.value) < 20:
                                velocity = 0
                        else:
                                velocity = copysign(gain * abs(e.value)**1.2, e.value)
                                if invert: velocity *= -1
                elif e.evtype == evinput.evtypes.KEY and e.code == 257 and e.value == 0:
                        stopped = not stopped
                        if stopped:
                                print "Stopped"
                        else:
                                stop = False
                                thrd = threading.Thread(target=update_loop)
                                thrd.setDaemon(True)
                                thrd.start()
                                print "Started"
