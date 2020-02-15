    //==============================================================================
    public class BluetoothManager
    {
        BluetoothManager()
        {
        }

        public String[] getMidiBluetoothAddresses()
        {
            String[] bluetoothAddresses = new String[0];
            return bluetoothAddresses;
        }

        public String getHumanReadableStringForBluetoothAddress (String address)
        {
            return address;
        }

        public int getBluetoothDeviceStatus (String address)
        {
            return 0;
        }

        public void startStopScan (boolean shouldStart)
        {
        }

        public boolean pairBluetoothMidiDevice(String address)
        {
            return false;
        }

        public void unpairBluetoothMidiDevice (String address)
        {
        }
    }

    //==============================================================================
    public class MidiDeviceManager
    {
        public MidiDeviceManager()
        {
        }

        public String[] getJuceAndroidMidiInputDevices()
        {
            return new String[0];
        }

        public String[] getJuceAndroidMidiOutputDevices()
        {
            return new String[0];
        }

        public JuceMidiPort openMidiInputPortWithJuceIndex (int index, long host)
        {
            return null;
        }

        public JuceMidiPort openMidiOutputPortWithJuceIndex (int index)
        {
            return null;
        }

        public String getInputPortNameForJuceIndex (int index)
        {
            return "";
        }

        public String getOutputPortNameForJuceIndex (int index)
        {
            return "";
        }
    }


    public MidiDeviceManager getAndroidMidiDeviceManager()
    {
        return null;
    }

    public BluetoothManager getAndroidBluetoothManager()
    {
        return null;
    }
