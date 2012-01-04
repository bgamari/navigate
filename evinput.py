#!/usr/bin/python

from array import array
from fcntl import ioctl
import struct
from collections import namedtuple
import io
import os.path

EVIOCGID = 0x80084502
EVIOCGNAME = 0x80ff4506 # len=255

Event = namedtuple('Event', 'tv_sec tv_usec evtype code value')

class Device(object):
        _event_struct = struct.Struct('LLHHi')
        _id_struct = struct.Struct('HHHH')
        DeviceId = namedtuple('DeviceId', 'bus_type vendor product version')

        def __init__(self, fname):
                if not fname.startswith('/dev'):
                        fname = '/dev/input/' + fname
                if not os.path.exists(fname):
                        raise RuntimeError("Device doesn't exist")
                self._file = io.FileIO(fname, 'w+')

        def read_device_name(self):
                buf = array('B', [0]*255)
                b = ioctl(self._file, EVIOCGNAME, buf, 1)
                return buf.tostring()

        def read_device_id(self):
                """ TODO: I don't think this works properly """
                st = Device._id_struct
                buf = array('B', [0]*8)
                b = ioctl(self._file, EVIOCGID, buf)
                t = st.unpack(buf.tostring())
                return Device.DeviceId(*t)

        def read_event(self):
                st = Device._event_struct
                d = self._file.read(st.size)
                e = list(st.unpack(d))
                return Event(*e)

def find_key(d, value):
        for k,v in d.items():
                if value == v:
                        return k
        return None

def type_to_str(evtype):
        d = find_key(event_types.__dict__, evtype)

class evtypes:
        SYN			= 0x00
        KEY			= 0x01
        REL			= 0x02
        ABS			= 0x03
        MSC			= 0x04
        SW			= 0x05
        LED			= 0x11
        SND			= 0x12
        REP			= 0x14
        FF			= 0x15
        PWR			= 0x16
        FF_STATUS		= 0x17

class codes:
        # Relative Axes
        REL_X			= 0x00
        REL_Y			= 0x01
        REL_Z			= 0x02
        REL_RX			= 0x03
        REL_RY			= 0x04
        REL_RZ			= 0x05
        REL_HWHEEL		= 0x06
        REL_DIAL		= 0x07
        REL_WHEEL		= 0x08
        REL_MISC		= 0x09

        # Absolute Axes
        ABS_X			= 0x00
        ABS_Y			= 0x01
        ABS_Z			= 0x02
        ABS_RX			= 0x03
        ABS_RY			= 0x04
        ABS_RZ			= 0x05
        ABS_THROTTLE		= 0x06
        ABS_RUDDER		= 0x07
        ABS_WHEEL		= 0x08
        ABS_GAS			= 0x09
        ABS_BRAKE		= 0x0a
        ABS_HAT0X		= 0x10
        ABS_HAT0Y		= 0x11
        ABS_HAT1X		= 0x12
        ABS_HAT1Y		= 0x13
        ABS_HAT2X		= 0x14
        ABS_HAT2Y		= 0x15
        ABS_HAT3X		= 0x16
        ABS_HAT3Y		= 0x17
        ABS_PRESSURE		= 0x18
        ABS_DISTANCE		= 0x19
        ABS_TILT_X		= 0x1a
        ABS_TILT_Y		= 0x1b
        ABS_TOOL_WIDTH		= 0x1c
        ABS_VOLUME		= 0x20
        ABS_MISC		= 0x28
        ABS_MT_SLOT		= 0x2f
        ABS_MT_TOUCH_MAJOR	= 0x30
        ABS_MT_TOUCH_MINOR	= 0x31
        ABS_MT_WIDTH_MAJOR	= 0x32
        ABS_MT_WIDTH_MINOR	= 0x33
        ABS_MT_ORIENTATION	= 0x34
        ABS_MT_POSITION_X	= 0x35
        ABS_MT_POSITION_Y	= 0x36
        ABS_MT_TOOL_TYPE	= 0x37
        ABS_MT_BLOB_ID		= 0x38
        ABS_MT_TRACKING_ID	= 0x39
        ABS_MT_PRESSURE		= 0x3a

        # Buttons
        MISC		= 0x100
        b0		= 0x100
        b1		= 0x101
        b2		= 0x102
        b3		= 0x103
        b4		= 0x104
        b5		= 0x105
        b6		= 0x106
        b7		= 0x107
        b8		= 0x108
        b9		= 0x109

        MOUSE		= 0x110
        LEFT		= 0x110
        RIGHT		= 0x111
        MIDDLE		= 0x112
        SIDE		= 0x113
        EXTRA		= 0x114
        FORWARD		= 0x115
        BACK		= 0x116
        TASK		= 0x117

        JOYSTICK		= 0x120
        TRIGGER		= 0x120
        THUMB		= 0x121
        THUMB2		= 0x122
        TOP			= 0x123
        TOP2		= 0x124
        PINKIE		= 0x125
        BASE		= 0x126
        BASE2		= 0x127
        BASE3		= 0x128
        BASE4		= 0x129
        BASE5		= 0x12a
        BASE6		= 0x12b
        DEAD		= 0x12f

        GAMEPAD		= 0x130
        A			= 0x130
        B			= 0x131
        C			= 0x132
        X			= 0x133
        Y			= 0x134
        Z			= 0x135
        TL			= 0x136
        TR			= 0x137
        TL2			= 0x138
        TR2			= 0x139
        SELECT		= 0x13a
        START		= 0x13b
        MODE		= 0x13c
        THUMBL		= 0x13d
        THUMBR		= 0x13e

        DIGI		= 0x140
        TOOL_PEN		= 0x140
        TOOL_RUBBER		= 0x141
        TOOL_BRUSH		= 0x142
        TOOL_PENCIL		= 0x143
        TOOL_AIRBRUSH	= 0x144
        TOOL_FINGER		= 0x145
        TOOL_MOUSE		= 0x146
        TOOL_LENS		= 0x147
        TOUCH		= 0x14a
        STYLUS		= 0x14b
        STYLUS2		= 0x14c
        TOOL_DOUBLETAP	= 0x14d
        TOOL_TRIPLETAP	= 0x14e
        TOOL_QUADTAP	= 0x14f

        WHEEL		= 0x150
        GEAR_DOWN		= 0x150
        GEAR_UP		= 0x151

