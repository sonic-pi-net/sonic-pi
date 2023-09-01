// MIT License

// Copyright (c) 2016-2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <stdbool.h>
#include <erl_nif.h>

#ifdef WIN32
    #define DllExport   __declspec( dllexport )
#else
    #define DllExport
#endif

#ifdef __cplusplus
extern "C" {
#endif



    DllExport int sp_midi_is_nif_loaded(bool* is_nif_loaded);
    DllExport int sp_midi_is_nif_initialized(bool* is_nif_initialized);

    // TODO: These are exported for C tests. Once we are happy that it's working they should not be exported
    /**
     * Initialize the spmidi library. Must be called before anything else.
     *
     * @return 0 if ok, < 0 if error
     */
    DllExport int sp_midi_init();

    /**
     * Deinitialize the spmidi library.
     */
    DllExport void sp_midi_deinit();

    /**
     * Send a MIDI message to the MIDI outputs.
     *
     * @param device_name: the name of the target device
     * @param c_message: pointer to the message (this is the MIDI binary message that might contain 0s in the middle)
     * @param size: size of the message. This is required since we cannot count on 0s to indicate its end
     */
    DllExport int sp_midi_send(const char *device_name, const unsigned char *c_message, unsigned int size);

    /**
     * Get the list of output devices.
     *
     * @param n_list: output parameter to indicate the number of devices found
     * @return the list of devices as a pointer to a pointer to chars
     */
    DllExport char **sp_midi_outs(int *n_list);

    /**
     * Get the list of input devices.
     *
     * @param n_list: output parameter to indicate the number of devices found
     * @return the list of devices as a pointer to a pointer to chars
     */
    DllExport char **sp_midi_ins(int *n_list);




    /************** Functions for the erlang integration below ***************/

    /**
     * Send the MIDI in event to the erlang process as binary data. Will use enif_send() to send
     * the data to the erlang process
     *
     * @param device_name: name of the device that got the data
     * @param data: pointer to the message (this is a binary message that might contain 0s in the middle)
     * @param size: size of the message. This is required since we cannot count on 0s to indicate its end
     * @return the list of devices as a pointer to a pointer to chars
     */
    int send_midi_data_to_erlang(const char *device_name, const unsigned char *data, size_t size);

    // Erlang NIFs. The NIF parameters are always the same, I will only explain the parameters as unpacked from erlang.
    // Note that the only NIF that passes data is sp_midi_send_nif(), the rest do not pass anything, and are simple
    // wrappers for the C functions that do the real work.
    DllExport ERL_NIF_TERM sp_midi_is_nif_loaded_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    DllExport ERL_NIF_TERM sp_midi_is_nif_initialized_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Initialize the spmidi library. Must be called before anything else.
     *
     * @return 0 if ok, < 0 if error
     */
    DllExport ERL_NIF_TERM sp_midi_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Deinitialize the spmidi library.
     */
    DllExport ERL_NIF_TERM sp_midi_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Send a MIDI message to the MIDI outputs.
     *
     * The erlang side passes a binary with the device name and the MIDI message.
     */
    DllExport ERL_NIF_TERM sp_midi_send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Get the list of output devices.
     *
     * It returns a string list to erlang.
     */
	DllExport ERL_NIF_TERM sp_midi_outs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Get the list of input devices.
     *
     * It returns a string list to erlang.
     */
    DllExport ERL_NIF_TERM sp_midi_ins_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Trigger a refresh of the MIDI devices. This is triggered from erlang when it a change in the topology is detected
     *
     */
    DllExport ERL_NIF_TERM sp_midi_refresh_devices(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * This must be called from the MIDI processing process in erlang, so that the C side can capture its PID.
     * Note it is not necessary that the erlang side passes its PID explicitly, the C side fetches it using enif_self().
     *
     */
    DllExport ERL_NIF_TERM sp_midi_have_my_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_midi_get_current_time_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    // Aux helper function
    ERL_NIF_TERM c_str_list_to_erlang(ErlNifEnv* env, int n, char** c_str_list);
#ifdef __cplusplus
}
#endif

