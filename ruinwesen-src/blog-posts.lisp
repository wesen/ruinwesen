(in-package :ruinwesen)

(defparameter *rw-blog* (blog-with-name "ruinwesen-blog"))
(defparameter *rw-news* (blog-with-name "ruinwesen-news"))

(post-to-blog *rw-blog* "MachineDrum Notes"
	      "<div align=\"center\"><object width=\"425\" height=\"344\"><param name=\"movie\" value=\"http://www.youtube.com/v/EQpdwD_oxI8&hl=de&fs=1\"></param><param name=\"allowFullScreen\" value=\"true\"></param><embed src=\"http://www.youtube.com/v/EQpdwD_oxI8&hl=de&fs=1\" type=\"application/x-shockwave-flash\" allowfullscreen=\"true\" width=\"425\" height=\"344\"></embed></object><br/></div>
<p>I am very proud to announce a new firmware for the MIDI Command which allows you to play the MachineDrum as a 16-voice polyphonic synthesizer. Just plug the MIDI Command into the MachineDrum and add a MIDI keyboard, and play all the machines you ever dream of as a polyphonic synthesizer. Discover how you can turn a snare drum into an awesome Rhodes sound, how you can layer noise with samples to emulate wavescanning monomachine sounds, and much much more.</p>"
	      "<p>In addition to being available on the MIDI Command, you can try out the new features with a small PC and Mac program that allows you to use the MachineDrum as a synthesizer in conjunction with your PC. Download it on our <a href=\"http://ruinwesen.com/support\">support page</a>. The zip file containing the MacOSX and Windows version can be found here: <a href=\"http://ruinwesen.com/support-files/MachineDrumNotes-0.1.zip\">MachineDrumNotes-0.1.zip</a>, and the user manual here: <a href=\"http://ruinwesen.com/support-files/MachineDrumNotes.pdf\">MachineDrumNotes.pdf</a>.</p>")
	      
(post-to-blog *rw-blog* "OSXMidiSPI Wrapper for OSX"
	      "MIDI in Java on OSX is not supported, as you may have noticed if you tried Java applications with MIDI. They usually only show the Java Realtime Sequencer and the Java Sound Synthesizer. In order to add MIDI support in Java, you either had to access the MIDI Subsystem through the deprecated CoreMidi API in Java, or use a wrapper. However, when moving to intel, there was some trouble with existing wrappers. Furthermore, I need to do pretty extensive Sysex processing and there were problems as well, so I decided to write my own wrapper for the Nomad Nord Modular Editor project. I put up the wrapper as a separate zip in order to support MIDI development under Java. You can get the file at http://ruinwesen.com/support-files/osxmidispi.zip ."
	      "The zip file contains two files libosxmidispi.jnilib and osxmidispi.jar which you have to copy to the folder /Library/Extensions/Java under OSX. The sourcecode for osxmidispi can be found in the CVS directory of Nomad, over at http://nmedit.sf.net/ (which is a wicked project by my friend Christian, check it out!). This will allow you to access OSX Midi Devices using the javax.sound.midi API. Enjoy!")

(post-to-blog *rw-blog* "RWMidi Processing MIDI Library"
	      "<p>I do some quite extensive MIDI development using the awesome processing environment (check it out at http://processing.org/ ), but one thing that always left me unsatisfied was MIDI support. This comes both from the broken JAVA Midi support under OSX (check the previous post on osxmidispi), as well as the existing MIDI libraries being too complicated for my taste. Also, the different MIDI implementations on Linux, Windows, OSX would lead to weird behaviour inside javax.sound.midi as well, which was most easily noticed when handling SYSEX messages. I finally resorted to write my own simple and clean MIDI wrapper for processing, which I am proud to announce to you now. You can download it at http://ruinwesen.com/support-files/rwmidi.zip . Simply unzip the file, and move the rwmidi folder to your processing libraries folder. The javadoc and a sketch example can be found at http://ruinwesen.com/support-files/rwmidi/documentation/RWMidi.html .</p>"
	      "<p>The library provides access to existing MIDI devices through static methods in the RWMidi class. It differentiates between input and output device, because some OS provide devices that can do both input and output, while others don't. Also, we don't provide access to devices through IDs, because these could change dynamically. The best way is to get an array with MidiInputDevices or MidiOutputDevices and access MIDI endpoints through these objects. For example:<p>

<pre>
  input = RWMidi.getInputDevices()[0].createInput(this);
  output = RWMidi.getOutputDevices()[0].createOutput();
</pre>

<p>You can also get a list of device names and access a device through its name (beware of device having similar names). After you get the MidiInputDevice and MidiOutputDevice, you have to open a MidiInput, which is used to receive data, and a MidiOutput, which is used to send data. You can either subclass MidiInput and override the send() method, or you can register callbacks with the MidiInput. The easiest way to do this is to provide an Object to the createInput() method. This will register common methods as callbacks: noteOnReceived(Note) is called when a Note ON is received, and is given a Note object as argument. Similarly, noteOffReceived(Note) for Note OFF, controllerChangeReceived(Controller) for CC messages, programChangeReceived(ProgramChange) for program change messages, and sysexReceived(SysexMessage) for sysex messages. Sysex Messages are always transmitted in full, you won't get half of a sysex message, but always a nice complete one.</p>
<p>Similarly, the MidiOutput object provides a number of methods to facilitate sending MIDI Messages: sendNoteOn, sendNoteOff, sendControllerChange, sendSysex, etc...</p>
<p>The library was tested under Linux, Windows XP and MacOSX, and seems to work fine. However, this is an early version, so any bugs you find, let me know!</p>")


(post-to-blog *rw-blog* "Debouncing buttons on the AVR"
	      "<p>While building the MUTE functionality, I noticed my buttons were bouncier than I thought, and the caps I used on the prototypes did not really protect against that. So I resorted to fix the problem in software rather than go back for another round of soldering. I read the status out of a shift register in a polling routine. Before further passing down this data to the GUI code, I introduced my debouncing code.</p>"
	      "<p>The basic idea is to filter out quick status changes. I have a timer clock running in an interrupt (it is a pretty slow timer), so the idea was to store the button status and a timestamp of the last status change. If the last status change is longer ago than a specific time change, then the bouncing has stopped and we can update the status bit. Here is the code to do that:</p>

<pre>volatile uint16_t timer2_slowclock = 0;

uint16_t clock_diff(uint16_t old_clock, uint16_t new_clock) {
  if (new_clock >= old_clock)
    return new_clock - old_clock;
  else
    return new_clock + (65535 - old_clock);
}
</pre>

<p>This is the code used to do time difference calculation taking into account wrapping of the clock register (a 16 bit value). Here is the code of the data structure I use:</p>

<pre>
typedef struct debounce_s {
  uint16_t stamp;
  uint8_t status;
} debounce_t;

debounce_t debounce[NUM_BUTTONS];
#define DEBOUNCE_TIME 1
</pre>

<p>The clock being pretty slow, the smallest timespan I allow is of just 1. I store one status and one timestamp for each button. Finally, here is the routine debouncing a register, given the number of buttons in the register and the start of the structure in the debounce array (this is becuase I have buttons connected to multiple sources). The routine returns the debounced version of the input value.</p>

<pre>
uint8_t dbounce(uint8_t but, debounce_t *bounce, uint8_t num) {
  uint16_t time;
  uint8_t i;
  uint8_t dbut = 0;

  cli();
  time = timer2_slowclock;
  sei();

  for (i = 0; i < num; i++) {
    if (IS_BIT_SET(but, i) != bounce[i].status) {
      if (clock_diff(bounce[i].stamp, time) > 100) {
	bounce[i].stamp = time;
	bounce[i].status = IS_BIT_SET(but, i);
      }
    }
    if (bounce[i].status) {
      SET_BIT(dbut, i);
    } else {
      CLEAR_BIT(dbut, i);
    }
  }
  return dbut;
}
</pre>

<p>Et voila, no more flickering and annoying mute buttons!</p>
")
(post-to-blog *rw-news* "Ruin & Wesen Live on Saturday"
	      "Wesen will play a monomachine/machinedrum liveset on webcam on saturday 27. September 2008, at 23:00 GMT (01:00 CET, 19:00 Eastern Time, 18:00 Central Time). He will demo the MidiCommand and the MonoJoystick, and show some secret new features! Watch it at http://ruinwesen.com/live ."
	      "")
(post-to-blog *rw-news* "Small bugfix update for RWMIDI"
	      "Fixed a bug where Controller messages with value 0 would not be recognized.<br/>Fixed a bug where sometimes sysex messages would not be fully parsed under MacOSX.<br/>Update available at http://ruinwesen.com/support ."
	      "")
(post-to-blog *rw-news* "Release MachineDrum Notes Java application"
	      "Turn your MachineDrum into a 16-voice polyphonic synthesizer! A whole new universe of sonic madness! For more details, read our blog at http://ruinwesen.com/blog?id=109 !" "")
(post-to-blog *rw-news* "Updated rwmidi and osxmidispi"
	      "Both rwmidi and osxmidispi have been updated to have better MIDI and Sysex support under OSX"
	      "")
(post-to-blog *rw-news* "MonoJoystick Handbuch auf Deutsch"
	      "Das <a href=\"/support-files/mono-joystick-deutsch.pdf\">MonoJoystick Benutzerhandbuch</a> ist jetzt auf Deutsch erh&auml;ltlich. The <a href=\"/support-files/mono-joystick-deutsch.pdf\">MonoJoystick Manual</a> is now available in German. The English Manual will soon be updated to reflect the new features."
	      "")

(post-to-blog *rw-news* "RWMidi Library Released"
	      "RWMidi is a small and nice MIDI library for Java and Processing. Get it at http://ruinwesen.com/support ."
	      "")

(post-to-blog *rw-news* "Mute on the MonoJoystick"
	      "The MonoJoystick now feature a MUTE mode that allows you to mute and unmute track son the MonoMachine. It features a third button now."
	      "")

(post-to-blog *rw-news* "Website launched"
	      "Weeeh this place is now for everybody! Have fun!"
	      "")
(post-to-blog *rw-news* "MonoJoystick Product Video"
	      "Here is a nice little video showcasing the current status of the MonoJoystick: http://www.youtube.com/watch?v=JP5x7jDg73c" "")

(post-to-blog *rw-news* "Coming Soon: Virtual Controllers"
	      "I am working on virtual versions of the MidiCommand and of the MonoJoystick, so you can try them out on the PC and see their functionality for yourself. Beta versions up soon!" "")

(post-to-blog *rw-news* "MonoJoystick Manual Online"
	      "I uploaded the preliminary version of the MonoJoystick manual. The firmware is still getting tweaked, so new features will be added." "")

(post-to-blog *rw-news* "CTR-AL for the Monomachine"
	      "At the request of a client, I added a CTR-AL kind of functionality to the MonoJoystick. Pressing a button on the MonoJoystick will change all the parameters on all the tracks. Furthermore, pressing down selected tracks will send the parameters only to those pages. It's like a sniper CTR-AL." "")

(post-to-blog *rw-blog* "AVR Assembly optimization"
	      "<p>I have built a MIDI controller based on the atmega8 processor by atmel. The processor itself has 1 kB of RAM and 8 kB of ROM. 1kB of that ROM is used by the bootloader, which is used to transmit new firmware images over sysex. As the sysex decoding and checksumming is quite complex, I could just barely fit the bootloader in those 1024 bytes.</p>"
	      "<p>I first wrote the bootloader in C, using the avr-gcc compiler (version 4.2.0). Sadly, the resulting code was just slightly over the 1024 bytes boundary I had to meet (you can only program the bootloader start adress on 512 bytes, 1024 bytes or 2048 bytes). That led to my first dabbling in optimizing gcc created code. I have to admit I don't start writing assembly from scratch, even now I write the main framework in C, and then rewrite specific functions in assembler. You can see the C source code of the bootloader here (though it seems this version fits in 1024 bytes, I must have done something to it too) :http://bl0rg.net/~manuel/midi-command/bootloader.c . I won't go into the bootloader details in this post (this will be the subject of a later post). Here is the assembler version: http://bl0rg.net/~manuel/midi-command/bootloader-asm.s .</p>

<p>First thing when trying to optimize for size is checking the size of the file. I do this using the tool avr-size:</p>

<pre>
stechapfel:midi-kontrol manuel$ avr-size bootloader.o
   text	   data	    bss	    dec	    hex	filename
    902	     12	      4	    918	    396	bootloader.o
</pre>

<p>This tells us that the .text section (program code) of the file is 902 bytes big, the .data section (initialized section) is 12 bytes, and the .bss (uninitialized data section) is 4 bytes big. The bss does not need to be stored in the ROM, so the final space taken up in ROM by our file is 924 bytes. While working on the main firmware, I wrote myself a small script to calculate how many ROM bytes I had left out of the 7 kB available:

<pre>
stechapfel:midi-kontrol manuel$ cat left.sh 
#!/bin/sh

avr-size $1 | tail -1 | cut -c1-20 | (read i j&& echo $((1024 * 7 - (i + j))) \"bytes left\")
</pre>

This way, I can always check how much space is available, for example:

<pre>
stechapfel:midi-kontrol manuel$ ./left.sh midi-kontrol.hex 
36 bytes left
</pre>

which shows that my firmware is pretty tight indeed.</p>

<p>Once we have the size information about the complete firmware, we need to check which functions are biggest, in order to know which should be optimized first. I do this using the avr-nm utility:</p>

<pre>
stechapfel:midi-kontrol manuel$ avr-nm --size-sort bootloader.o
00000001 B block_cnt
00000001 B in_sysex
00000001 C sysex_cnt
00000002 B jump_to_app
00000006 D ack_msg
00000006 D nak_msg
00000008 T bl_uart_putc
0000000a T bl_uart_getc
00000018 T midi_sysex_send_ack
00000018 T midi_sysex_send_nak
0000001c T jump_to_main_program
0000001e T write_firmware_checksum
00000036 T handle_sysex
0000003a T check_firmware_checksum
0000003e T write_checksum
00000040 C sysex_data
00000046 T make_word
00000064 C data
00000070 T main
0000007e T handle_midi
00000128 T write_block
</pre>

<p>The first column shows the size of a symbol in hexadecimal, the second column shows the section of the symbol (T is text, C is unitialized, B is bss, and D is data). This shows which function might lead itself to optimization. For the bootloader, I just had to scrape off a few bytes to make it fit, so I just skimmed over the assembly source and kind of \"peephole-optimized\" by hand. You can see in the bootloader-asm.s that it is not hand-written assembler, but the output from gcc which has been modified. To generate the assembler listing out a C file, use the -fverbose-asm -S flags for gcc, along with the other flags you have. I wrote a Makefile rule to make just that:</p>

<pre>
%.s: %.c
	$(CC) -S $(CFLAGS) -fverbose-asm $< -o $@
</pre>

I also generate an assembler listing along with every .c file I compile to check things later on. Here is my normal compile rule:

<pre>
%.o: %.c Makefile
	$(CC) $(CFLAGS) -Wa,-adhlns=$@.lst -c $< -o $@


%.o: %.s Makefile
	$(CC) $(CFLAGS) -Wa,-adhlns=$@.lst -c $< -o $@

</pre>

<p>Let's look at the code avr-gcc generates for the bootloader.c file: http://bl0rg.net/~manuel/midi-command/bootloader.s. As you can see gcc includes the original sourcecode along with the generated assembler. Sometimes they don't line up quite well, so you have to read between the lines.</p>

<p>Let's take a bit of time to explain the register usage of avr-gcc. The atmega8 has 32 registers, from r0 to r31. Each register is 8 bit. C data types are: char 8 bit, int 16 bits, long 32 bits, long long 64 bits. </p>

<p>r0 and r1 are fixed registers: r0 is the temporary registers, which can be overwritten by any function (except interrupts). Use it to store temporary stuff. r1 is the zero register and should always be cleared (you can use it for something else temporarily, but must make sure it's clear when you're done). This has bitten me a few times, because the mul operand writes into r1, and I would forget to clear it, wondering what weird things my avr would do afterwards.</p>

<p>r18-r27 and r30-r31 are \"call-used registers\", you can use them for whatever you want in your own functions, but must be aware that they can be clobbered by any C function you call. These registers are very useful because your own functions can actually clobber them too without taking care of them. You don't have to push them on entering your function, and popping them on leaving. Also, when calling your own assembly routines, you know which are going to get clobbered and which don't. The main work around optimizing for space is working within the boundaries of the call-used registers. Each register that you have to push/pop will take up 4 bytes of ROM (actually a bit less if you use a jumpable epilogue, which I will get into later).</p>

<p>r2-r17 and r28-r29 are call-saved registers, which you have to push on entry and pop on exit of your function. If your function is only called by assembler code, you can forego these limitations and track register usage by hand. The advantage of these registers is that once you are using them, you can use them to store data and call C functions without having to worry about them.</p>

<p>Function calls use the register pairs r25:r24, r23:r22, r21:r20 down to r9:r8. Additional arguments are passed on the stack. Return values are passed in r25:r24. The register pairs r27:r26, r29:r28 and r31:r30 can be used to indirectly address memory. This is very important because clever array addressing loops are the main way to optimize C code for space.</p>

<p>Now that this is clear, let's go through the bootloader.s file, and I'll write down what I notice. It's been 1 month since I actually optimized that file, so I don't remember what I exactly did.</p>

<p>The first defined function is check_firmware_checksum. I'll go into a bit of detail to show how a function is defined in assembler so that the avr-* tools can work with it.</p>

<pre>
 101               		.section	.text.check_firmware_checksum,\"ax\",@progbits
 103               	.global	check_firmware_checksum
 105               	check_firmware_checksum:
</pre>

<p>The .section directive is important and is used to put each function into its own section. This is used later on by the linker with the --gc-sections option, which removes every non-referenced function from the final firmware file. .global declares the symbol as a global symbol so that other files can link against it. Finally check_firmware_checksum: defines the actual function entry point. THe size information is now stored in .stabd sections if I understand this correctly, however I use the .size check_firmware_checksum,.-check_firmware_checksum directive at the end of the function. It defines the size of the symbol check_firmware_checksum as the difference between the current address and the function start address. This is important so that you can check the size of your assembler functions. By the way, __eeprom_read_word_1C1D1E doesn't follow normal C calling semantics, in case you were wondering. </p>

<p>By going through the first functions, we see that avr-gcc does a quite good job at optimizing for space. In midi_sysex_send_ack we see how avr-gcc correctly replaces the loop index by a check on the incremented pointer. In make_word, we can use our first surgical code excision:</p>

<pre>
 499 0024 E62F      		mov r30,r22
 500 0026 FF27      		clr r31
 501 0028 E050      		subi r30,lo8(-(data))
 502 002a F040      		sbci r31,hi8(-(data))
 503 002c 2081      		ld r18,Z
 504 002e 3327      		clr r19
 505 0030 4427      		clr r20
 506 0032 5527      		clr r21
 507 0034 282B      		or r18,r24
 508 0036 392B      		or r19,r25
 509 0038 4A2B      		or r20,r26
 510 003a 5B2B      		or r21,r27
</pre>

<p>r22 is the loop counter, incremented by one every iteration.  We could remove the need for r22 altogether, charge up r30 as loop counter, and check that for the boundary at the end. Also, the clr and or magic is unnecessary, as we could just move r25 into r19, and let the data stay in r26. The way it looks in bootloader-asm.s, it seems that was not the point where I optimized. I do realize now that I must have used an older version of gcc at the time, because the new code is quite approaching perfect, the major optimization I can find in my assembler code is avoiding to clear both r24 and r25 when returning an 8 bit value. I guess real optimization things will have to wait for a second post, so this post is more of an introduction into assembler on the AVR than real optimization magic.</p>
")

(post-to-blog *rw-blog* "Encoding 8 bit Data in MIDI Sysex"
	      "<p>When sending patch data over sysex, you pretty quickly encounter the problem of having to encode 8 bit data over the 7 bit values sysex offers you. I use the pretty basic approach of adding an additional byte every 7 bytes to store the MSB of every value. So 7 values look like this when sent over sysex:</p>"
	      "

<pre>(0 MSB7 MSB6 MSB5 MSB4 MSB3 MSB2 MSB1)
 (BYTE1 & 0x7F) (BYTE2 & 0x7F) (BYTE3 & 0x7F) 
 (BYTE4 & 0x7F) (BYTE5 & 0x7F) (BYTE6 & 0x7F) 
 (BYTE7 & 0x7F) 
</pre>

<p>The code to encode values in C is:</p>

<pre>uint8_t data_to_sysex(uint8_t *data, uint8_t *sysex, uint8_t len) {
  uint8_t retlen = 0;
  uint8_t cnt;
  uint8_t cnt7 = 0;

  sysex[0] = 0;
  for (cnt = 0; cnt < len; cnt++) {
    uint8_t c = data[cnt] & 0x7F;
    uint8_t msb = data[cnt] >> 7;
    sysex[0] |= msb << cnt7;
    sysex[1 + cnt7] = c;

    if (cnt7++ == 6) {
      sysex += 8;
      retlen += 8;
      sysex[0] = 0;
      cnt7 = 0;
    }
  }
  return retlen + cnt7 + (cnt7 != 0 ? 1 : 0);
}
</pre>

<p>This code assumes the sysex buffer is big enough to hold the values (not much space for safe programming on embedded platforms :). The main loop encodes the msbs of 7 bytes, and every 7 bytes advances 8 bytes in the destination buffer. The final test is there to remove a trailing 0 byte.</p>

<p>The same code in python (used in the ableton controller code):</p>

<pre>def data_to_sysex(data):
    sysex = [0]
    idx = 0
    cnt7 = 0

    for x in data:
        c = x & 0x7F
        msb = x >> 7
        sysex[idx] |= msb << cnt7
        sysex += [c]

        if cnt7 == 6:
            idx += 8
            sysex += [0]
            cnt7 = 0
        else:
            cnt7 += 1

    if cnt7 == 0:
        sysex.pop()
        
    return sysex
</pre>

<p>which basically does exactly the same. I haven't written python code for almost a year now, so I'm a bit rusty, so maybe there is a more elegant way to code this :)</p>

<p>Decoding is the same process in reverse:</p>
<pre>uint8_t sysex_to_data(uint8_t *sysex, uint8_t *data, uint8_t len) {
  uint8_t cnt;
  uint8_t cnt2 = 0;
  uint8_t bits = 0;
  for (cnt = 0; cnt < len; cnt++) {
    if ((cnt % 8) == 0) {
      bits = sysex[cnt];
    } else {
      data[cnt2++] = sysex[cnt] | ((bits & 1) << 7);
      bits >>= 1;
    }
  }
  return cnt2;
}
</pre>

<p>The bits are store at every 8 byte boundary, and the 7 following bytes are restored by shifting through the bits variable. Once you received a sysex packet, you can just decode data like this (this is out of the controller code):</p>

<pre>void receive_page_sysex(void) {
  if (sysex_data[4] == curpatch) {
    uint8_t page = sysex_data[5];
    uint8_t type = sysex_data[6];
    sysex_to_data(sysex_data + 7, 
                  get_encoder_page_data(page),
                  sysex_cnt - 7);
    get_encoder_page(page)->type = type;
    current_page->refresh = 1;
    if (page == curpage) {
      set_page(page);
      flash_active = 0;
    }
  }
}
</pre>

<p>It checks a bit of stuff in the sysex message itself (bytes 4, 5 and 6), and then decodes the data directly into the right buffer. Again, there is no check whatsoever done here, so you can easily overwrite memory by sending the wrong message, but I don't think there are hackers interested in owning MIDI controllers running around with rogue MIDI devices :)</p>
")
	      
(post-to-blog *rw-blog* "Writing Ableton Control Surface Scripts"
	      "<p>I've been dabbling with ableton control surface scripts about one year ago (some people may have seen the FCB1010 control surface script: http://bl0rg.net/~manuel/fcb1010-mappings/ , which I don't really have the time to support at the moment). Btw I have a new version of the mappings, but they're not really documented, but if you're a bit into programming you may be able to get the mappings from the consts.py file: http://bl0rg.net/~manuel/FCB1010-mappings.zip . In the following, I assume you know how to use remote control surface mappings inside ableton (the whole Midi Devices Input Output thingie).</p>"
	      "<p>But let's get down to the business of writing ableton remote control surface scripts. The scripts are written in Python and are stored inside the Ableton Live application in the Resources folder under Windows, or inside the application bundle under MacOSX. The directory containing the scripts is called \"MIDI Remote Scripts\". As you can see in this folder, there is an additional folder for each supported device. These folders containing the actual scripts. I'm writing the scripts for my midi controller called MidiCommand, so I created a directory called \"MidiCommand\" inside this folder. The MidiCommand folder will contain my script. You can download my files at bknr:/support-files/midi-command-mappings.zip</p>

<p>The main problem with developing Ableton scripts is that you have no direct interface to debugging the scripts. Poking around the ableton application, you can find a hidden menu containing options to open a python console and reload scripts, but I haven't tried to hook into these functions (I suppose that's what people at Ableton use). Instead, I wrote my own kind of \"debugging\" functions. This is not perfect though. If your python code contains semantical errors or import errors, Ableton will not load your script and you are left on your own. One way to check for typos is to compile the python files before copying them into the Ableton folder. You have to use python2.2 for this, which I sadly didn't get to compile on Macosx Leopard. I used this script when I was on Macosx 10.4. py-compileall.py, which compiles all the files in the directories given in the command line.</p>

<pre>
#!/usr/bin/env python22

import os, sys
import py_compile
import compileall

def main():
    if len(sys.argv) < 2:
	print \"usage: compileall tree ...\"
	return 0
    else:
	for dir in sys.argv[1:]:
	    print \"compiling %s\" % dir
	    compileall.compile_dir(dir)
	return 1

if __name__ == '__main__':
    exit_status = not main()
    sys.exit(exit_status)

</pre>

<p>If you just copy the python files into the folder, Ableton will compile them for you.</p>

<p>The structure of a script is as follows: Ableton looks for a file called __init__.py and loads it. This file returns an object that will be used as the script \"controller\". My __init__.py file for the MidiCommand control surface looks like this:</p>

<pre>
import Live
from MidiCommand import MidiCommand

def create_instance(c_instance):
    return MidiCommand(c_instance)
</pre>

<p>As you can see, I import the Live module (which doesn't exist in Ableton Live 5, so I wrote my own version for Live 5) which is provide by Ableton Live, and also import the main object of the script, which is MidiCommand. All the real work is done inside this module. If you want to get used to scripts, try to write a simple __init__.py that will just create a file in /tmp and write something into it. This way, you can see if your script setup works fine. Another problem is reloading script when you have changed it. I just quit Ableton and restart it, which is a bit annoying, but once Ableton is cached it loads quickly.</p>

<p>Let's have a look at the MidiCommand.py file:</p>

<pre>
import Live
from MidiCommandScript import MidiCommandScript
from MidiCommandMixerController import MidiCommandMixerController
from MidiCommandDeviceController import MidiCommandDeviceController
from consts import *

class MidiCommand(MidiCommandScript):
    __module__ = __name__
    __doc__ = 'Automap script for MidiCommand controllers'
    __name__ = \"MidiCommand Remote Script\"
    
    def __init__(self, c_instance):
        self.suffix = \"\"
	MidiCommand.realinit(self, c_instance)

    def realinit(self, c_instance):
	MidiCommandScript.realinit(self, c_instance)
	self.mixer_controller = MidiCommandMixerController(self)
	self.device_controller = MidiCommandDeviceController(self)
        self.components = [ self.mixer_controller, self.device_controller ]

    def suggest_map_mode(self, cc_no):
        return Live.MidiMap.MapMode.absolute
</pre>

<p>I have separated the __init__ method in a second part \"realinit\", which is because I use a weird debugging mechanism to trace calls (I think that's the reason, to be honest I don't remember it quite clearly). The MidiCommand inherits from a class MidiCommandScript which is a quite \"general\" wrapper for script functions. The c_instance parameter is a Boost Python object that is used to communicate with the C++ core of Ableton Live. The script then instantiates two further objects, the Mixer Controller and the Device Controller. Real work gets delegated to these 2 objects. Mixer Controller is responsible for handling mappings relating to tracks, while the Device Controller handles device specific mappings. I usually have a third controller for the transport, but my MidiCommand doesn't support these functions (yet). As you can see, this class doesn't contain much real code either, all the work is actually handled by MidiCommandScript, which delegates most function calls to the two controllers. Let's have a look at MidiCommandScript (just excerpts, as the file is way more complicated):</p>

<pre>
from Tracing import Traced
...

class MidiCommandScript(Traced):
...
    __filter_funcs__ = [\"update_display\", \"exec_commands\", \"log\", \"song\"]

</pre>

<p>This is actually the biggest piece of voodoo I use to debug my scripts. The Traced class, defined in Tracing, is a metaclass that will log every method call of classes inheriting from it in a special file. This way, I can trace my function calls and see when a call goes wrong (casts an Exception). The __filter_funcs__ field is there to filter out functions that are not really interesting or that are called very often. I took most of the code for the Tracing class from this python essay: http://www.python.org/doc/essays/metaclasses/Trace.py . It will log all method calls in the file /tmp/fftrace.log under Macosx and C:/tmp/fftrace.log under Windows. If you are interested in the workings of Tracing.py, feel free to read the essay I linked, I don't really remember the semantics. I have to say though that this kind of meta-hackery was quite a pain compared to how easy that stuff is in Lisp (yes, managed to sneer at python and mention Lisp, my good deed of the day is done).</p>

<p>Continuing our investigation of MidiCommandScript.py, you can see in the realinit method that when __myDebug__ is true, a logfile is created. This is used as an additional way to log data out of Ableton for me to read while my script runs. To write things into the logfile, you can use the methods log and logfmt. You can also see that I reference a file called \"MidiCommand-cmd\". Commands written into this file are polled by my script in the exec_commands method. This method reads in the file, executes the statements, and then clears it. These 3 tools (fftrace.log, /tmp/midi-kontrol log, and commands in /tmp/midi-kontrol-cmd) are my main helps when debugging scripts. I found out about most of the API by looking at the other scripts in the Midi Remote Scripts folder, and looking at the sourcecode by using the \"decompyle\" utility to transform the python bytecode into something readable (mostly).</p>

<p>But let's get down to actual Ableton Live API stuff. All the lower methods in MidiCommandScript are part of the Ableton Live API. disconnect is called when removing the script or closing ableton. I just call disconnect on all the child components (the Mixer Controller and the Device Controller mentionned above). application returns the Python object representing the whole application, while song returns the Python object representing the current liveset. This object is used a lot, because it contains links to the tracks, devices, clips, scenes, etc... suggest_input_port and suggest_output_port can be used to automagically choose the right MIDI device. can_lock_to_devices tells Ableton if the script can \"hold on\" to a specific device. This is used by the Device Controller, which will then remember the locked device and not react to selecting new devices until the locked device is unlocked. For more information locking devices, read the Ableton Live Remote Control Surface documentation. The actual locking is done using the lock_to_device, unlock_to_device (sic) and toggle_lock methods. These calls are also just delegated to the child components. suggest_map_mode is used to tell Ableton which kind of CC data is sent by a specific control (relative, absolute, etc...). show_message is a helper to display an informational message in the status bar of ableton. request_rebuild_midi_map is a helper to tell Ableton that the script would like to reinitialize its midi mappings. This is closely related to build_midi_map, which is called by Ableton and allows the script to actually provide midi mappings. The handle passed as an argument is used by the script to register mappings. As you can see, this call is also delegated to the child components. The Mixer Controller will register mappings pertaining to tracks (like volume control, sends, and also channel EQs). update_display is called periodically by ableton to allow the script to do some regular tasks, update status displays etc... It calls the logging function and is then passed on to child components. send_midi is used to send a byte tuple as midi (useful to send sysex, for example). receive_midi is called by ableton when midi data is received on the script interface which is not directly handled by registered mappings. This way, you can receive sysex data, but also ask for ableton to forward note data and ccs from the device without having to register them to a specific parameter. The method does a bit of parsing by recognizing notes and ccs and calling the appropriate methods in the child components.</p>

<p>The actual MIDI mapping work likes this: Ableton calls build_midi_map, which allows the script to register mappings using the passed midi_map_handle. A registered mapping is a link from a midi parameter (Note or CC) to an element in the GUI of ableton. For example mixer parameters (volume, send, cue level, etc...), device parameters, or clip parameters. The script can also use the midi map handle to ask Ableton to forward specific ccs or notes to the script. This allows to do more specific interpretation of some data. For example, the MidiCommand script asks ableton to forward the relative CCs used to scroll through tracks and scenes, and interprets it in its own custom way.</p>

<p>Another interesting class used is DumpXML, which I use to dump the documentation of ableton as XML, and also ableton tracks. I used this a while ago by dumping my whole liveset structure, and using that to organize my drum machine patterns. I had an audio clip for each track and pattern in my drum machine, featuring a recorded a loop of the drummachine, and used this to reorganize my loops. Then, I would generate a sysex file for the drummachine containing the whole liveset structure organized as rows, and used that in the Song Mode of the drum machine. THis way, I didn't have to spend days in the editing mode of the drum machine to organize my hundreds of loops :) Sadly, I have found no way to access the arrangement view information out of a script.</p>

<p>Finally, the MidiCommandScript instantiates a helper class, MidiCommandHelper, which contains a bunch of useful methods for writing Ableton scripts and communicating with the MidiCommand. They for example contain the sysex packing I documented yesterday, and helper methods to initialize parameters on the MidiCommand. THere are also methods to select scenes, tracks, find the last EQ on a track channel, etc... All the methods should be pretty self explanatory. </p>

<p>After this tour of all the utility I have at my disposal to write scripts, let's look at the two files doing the actual work: MidiCommandDeviceController.py and MidiCommandMixerController.py. As written above, the Device Controller handles mapping pertaining to devices in Ableton (Compressor, Simpler, etc...). My MidiCommand has 4 pages with 4 encoders, and a small display able to display 3 characters per encoder (actually 4 characters, but one is used as whitespace to separate the fields). I map the 8 encoders on the last 2 pages to the Best-Of Bank parameters of the currently selected device. Also, when a new device is selected, I flash the name of the device on the MidiCommand. The key to the magic can be found in the build_midi_map method, which is called by Ableton when a new device is inserted, or a new device is selected. Actually the midi map building is triggered by the lock_to_device and unlock_from_device methods (which are called by the parent object MidiCommandScript, if you remember from a few paragraphs back). You can also add a listener to when a new device is selected, but Ableton already automatically calls build_midi_map when this happens. build_midi_map forwards the work to the method map_device_params, which checks if a device has been selected. If this is the case, it looks for the best-of parameters in dictionaries (which you can find in the file Devices.py). It then calls either map_params_by_name for Ableton internal devices, or map_params_by_number for VST or AU instruments. map_params_by_number just maps the first 8 parameters of a VST or AU. </p>

<pre>
	def map_params_by_number(device):
	    ccs = PAGE3_CCS + PAGE4_CCS
	    channel = CHANNEL_MIDI_KONTROL
	    mode = Live.MidiMap.MapMode.absolute
	    for encoder in range(8):
		# +1 to skip \"Device on\"
		if (len(device.parameters) >= encoder + 1):
		    ParamMap.map_with_feedback(midi_map_handle, channel, \
					       ccs[encoder], \
					       device.parameters[encoder + 1], \
					       mode)

</pre>

<p>The CC numbers used by MidiCommand are stored in PAGEx_CCS in consts.py. All I do is iterate over the device parameters, and call the helper method \"map_with_feedback\" defined in ParamMap.py, which defines a standard feedback rule and stores the mapping in the midi map handle. And that's basically it, all the further work will be done by Ableton itself. Sending a CC will update the parameter, and changing the parameter with the mouse will send out feedback CCs to the midi kontrol. map_params_by_name does basically the same, but looks up parameters using the names stored in the best-of bank tuple defined in Devices.py.</p>

<p>At the end of the build_midi_map method, I send the SYSEX to update the page on the MidiCommand (mostly to display the update parameters). To do this I use the methods I defined in MidiCommandHelper.py . Finally I send a flash message if a new device has been selected. Et voila, an automagic device mapping script for Ableton, that will display the name of the mapped parameters on the LCD of my MidiCommand.</p>

<p>The Mixer Controller also handles the navigation inside Ableton. I use two relative CCs to scroll scenes and scroll tracks. I can't map these directly through the script interface, so I have to interpret the MIDI data myself. I ask Ableton to forward the CCs to my script:

<pre>
	def forward_cc(chan, cc):
	    Live.MidiMap.forward_midi_cc(script_handle, midi_map_handle, chan, cc)
	idx = 0

        forward_cc(CHANNEL_MIDI_KONTROL, SCENE_SCROLL_CC)
        forward_cc(CHANNEL_MIDI_KONTROL, TRACK_SCROLL_CC)
</pre>

and handle received CCs in the method receive_midi_cc (which is called by the parent MidiCommandScript, look a few paragraphs back):

<pre>
    def receive_midi_cc(self, channel, cc_no, cc_value):
        def rel_to_val(rel):
	    val = 0
	    if (cc_value >= 64):
		val = cc_value - 128
	    else:
		val = cc_value
            return val
            
        if (channel != CHANNEL_MIDI_KONTROL):
            return
        
        if (cc_no == SCENE_SCROLL_CC):
	    val = rel_to_val(cc_value)
	    idx = self.helper.selected_scene_idx() + val
	    new_scene_idx = min(len(self.parent.song().scenes) - 1,\
                                    max(0, idx))
	    self.parent.song().view.selected_scene = \
                             self.parent.song().scenes[new_scene_idx]
        elif (cc_no == TRACK_SCROLL_CC):
            val = rel_to_val(cc_value)
            current_idx = self.helper.selected_track_idx()
            idx = current_idx + val
            tracks = self.parent.song().tracks +\
         self.parent.song().return_tracks + (self.parent.song().master_track,)
            new_track_idx = min(len(tracks) - 1, max(0, idx))
            track = tracks[new_track_idx]
            if (current_idx != idx):
                self.parent.song().view.selected_track = track

</pre>
</p>

<p>This method is a bit more complicated. First it checks if the CC received was on the correct channel, then converts the absolute CC value to a relative value (using the rel_to_val helper). It then selects the new scenes out of the self.parent.song().scenes tuple. Assigning the variable self.parent.song().view.selected_scene will change the selected scene in Ableton. The tracks updating is similar, except that we have to append normal tracks, return tracks and the master_track ourselves.</p>

<p>The mapping magic of the mixer controller is very similar to the mapping magic of the device controller. The first page of the MidiCommand contorls the volume of the first 4 tracks in Ableton, while the second page controls the volume, the first send and low and high-pass levels if an EQ is present on the channel. I go through the tracks, map to the volume (for the first page), and then send the update description of the first page. Then I go through the selected track, map the send if its present, and use helper methods to get hold off the last EQ present on the track (either a Filter8 or an EQ3), and map the high and low parameters to the last 2 encoders of the second page. Then I send the updated names to the MidiCommand, et voila!</p>

<p>Writing ableton control scripts isn't very difficult in theory, but as there is no documentation or development tools whatsoever, it can be a bit of a challenge sometimes. I hope this information will help you to create awesome mappings for all of us to enjoy :)</p>")
	      
(post-to-blog *rw-blog* "Sysex Bootloader for AVR"
	      "<p>I build a lot of small MIDI devices, and using a programmer often is not an option because the programming port is used for MIDI itself, or to connect other circuitry, and toggling jumpers is just not an option. Also, it is nice to have the option to have users of my hardware update the firmware themselves. The logical solution was thus to use MIDI itself to update the firmware. Download the C sourcecode of the bootloader here: http://bl0rg.net/~manuel/midi-command/bootloader.c</p>"
	      "<p>A bootloader for the AVR family resides in the upper regions of flash memory. A fuse can be programmed to tell the AVR to boot not from address 0x0000, but from the start address of the bootloader. Thus, the bootloader gets called before any application code, and is able to reprogram the device before executing code. This has two advantages for us: the obvious one of being able to program the device over MIDI, and the good thing to have a way to upgrade firmware if a bug is found in the code. The size of the bootloader portion can be programmed into the fuses as well. The bootloader can be 2048 words big (not possible on the atmega8 I'm using), 1024 words big (that's 2kB), 512 words, 256 words or 128 words big. I settled on the 512 words size, cause that just about fits my MIDI bootloader code. You can set the fuse bits using your atmel programmer. I use avrdude and the following command (out of the Makefile):</p>

<pre>
init:
#       enable watchdog, external crystal
# 1024 words bootloader
	avrdude -p m8 -P usb -c usbasp -U hfuse:w:0xd8:m -U lfuse:w:0xcf:m
init512:
# 512 words bootloader
	avrdude -p m8 -P usb -c usbasp -U hfuse:w:0xda:m -U lfuse:w:0xcf:m
</pre>

<p>The memory taken up by the bootloader is called \"No-Read-While-Write\" memory, while the lower memory used for storing the program flash is called \"Read-While-Write\" memory. What this means is that while writing to the RWW section, the CPU can read from the NRWW section. </p>

<p>Finally, we have to tell the linker the correct start address for the code, which I do by passing a flag to gcc:

<pre>
bootloader.elf: bootloader.o 
	$(CC) $(CLDFLAGS) -Wl,--section-start=.text=0x1C00 -o $@ $+
</pre>

0x1c00 is the start adress of the code here (0x1c00 = 0x2000 - 0x400).</p>

<p>Reprogramming the flash memory of the AVR is done page-wise. Each page is 64 bytes big. First a page needs to be erased, and then written all at once. avr-libc provides a few helper function to reprogam the flash. You have to be careful while reprogramming not to overwrite the bootloader itself. Here is the code doing the actual programming in my MIDI bootloader, extracted from the function write_block:</p>

<pre>
  uint8_t sreg = SREG;
  cli();
  boot_page_erase(sysex_address);
  boot_spm_busy_wait();
  uint16_t address = sysex_address;
  for (i = 0; i < 64; i+=2) {
    uint16_t tmp = sysex_data[i] | (sysex_data[i + 1] << 8);
    boot_page_fill(address, tmp);
    address += 2;
  }
  boot_page_write(sysex_address);
  boot_spm_busy_wait();
  boot_rww_enable();
  SREG = sreg;

</pre>

<p>First we disable interrupts, the erase the page at sysex_address, which is the address where we want to write. Waiting for the page to be erased is done by calling the boot_spm_busy_wait function. Then we fill the write buffer using the function boot_page_fill, and write the page using boot_page_write. Finally, we reenable reading from the RWW section calling boot_rww_enable. You can see in the loop above that boot_page_fill takes a word as argument, so we create 16 bit values out of the data in the sysex buffer (LSB first). Now that we know how to flash a page, let's see how the communication is handled.</p>

<p>On the microcontroller side, MIDI is just a serial bus. The MIDI input and output are connected to the TX and RX pins of the microcontroller. So why not just use a serial bootloader? That's because MIDI imposes certain restrictions on the data that can be sent over it. Bigger data messages have to be encapsulated in \"System Exclusive\" messages. These are messages that begin with 0xF0 and end with 0xF7. Due to the inband-signaling of MIDI using the MSB of each byte, we can only send 7-bit data in a sysex message. Thus we have to encode 8-bit data into 7-bit data as described in this previous blog post: http://blogs.bl0rg.net/netzstaub/2008/08/14/encoding-8-bit-data-in-midi-sysex/ .</p>

<p>We saw that writing to the flash is done page-wise, and that each page is 64 bytes big. It is pretty straightforward then to transmit 64 bytes of program data over sysex, and to flash them into a new page. Should flashing go wrong, the bootloader is still there to restart the whole process. However, we want to avoid flashing invalid data. Thus, we use a simple checksumming code in the sysex communication to increase the reliability of the sent data. We XOR each byte together and store that as a checksum at the end of the sysex data. This checksumming is done in write_block:</p>

<pre>
  uint8_t checksum = 0;
  uint8_t i;
  for (i = 3; i < sysex_cnt - 1; i++) {
    checksum ^= data[i];
  }

  uint8_t length = data[4];
  uint16_t sysex_address = make_word(5, 4);

  if (sysex_address >= APP_END) {
    return 0;
  }
  
  uint8_t cnt = 0, recvd = 0;
  uint8_t bits = 0;
  for (cnt = 0; cnt < (sysex_cnt - 9); cnt++) {
    if ((cnt % 8) == 0) {
      bits = data[9 + cnt];
    } else {
      sysex_data[recvd++] = data[9 + cnt] | ((bits & 1) << 7);
      bits >>= 1;
    }
    if (recvd >= length)
      break;
  }

  uint8_t check = data[sysex_cnt - 1];
  checksum &= 0x7f;

  if ((checksum != check) || (recvd != 64)) {
    return 0;
  }
</pre>

<p>We can see the checksumming in the first few lines. We get the length of the page out of the data packet, and create a 16 bit address out of 4 sysex bytes with make_word. Finally, we decode the sysex data as described in the previous blog post. We check both the checksum and the length of the received data before writing the block.</p>

<p>We have however another major problem to take care of. Writing to the flash is not fast, and we don't have enough memory to store a whole firmware in flash. We do actually just store the current page in memory. We have to find a way to throttle the sending of the firmware over MIDI to allow the device to write to its flash. This is done in a very simple way: each block writing is acknowledged with an ACK message over MIDI. The sender won't write another firmware block to MIDI before receiving the ACK message. </p>

<p>This was basically the whole MIDI part of the bootloader, for more detail check the bootloader.c file. handle_midi is a very simple state machine taking care of recognizing sysex messages and ignoring the rest. We have a few other issues to take care of first. How does the bootloader enter the real program once it has written it to flash memory. Actually, it's very easy, we just have to jump to the adress 0x0000. In C, we can declare a function pointer to be 0x0000.</p>

<pre>
void (*jump_to_app)(void) = 0x0000;
</pre>

<p>There is a special trick regarding interrupts. The interrupt vector table is normally stored at the beginning of the flash memory. But that is the RWW memory, while our code is executing in NRWW. There is a flag in GICR to move the interrupt table to the bootloader section, and we set this flag at the beginning of our main function:

<pre>
  /* move interrupts to bootloader section */
  GICR = _BV(IVCE) | _BV(IVSEL);
</pre>

This means that after moving to the \"real\" application code, we'll have to restore the interrupts to their normal location (out of main.c of the controller):

<pre>
  GICR = _BV(IVCE);
  GICR = 0;
</pre>
</p>

<p>Another issue is that we don't want to always reprogram our flash before executing our software. We want to execute the stored flash except when explicitly asking to reprogram (or when the firmware has become broken). At the start of our device, we check if a button is pressed. If it is, we stay in the bootloader, else, we call the main program. Additionally, we want to signal the device that it is waiting for a reflash while running our \"main\" application. We do this by writing a special byte into EEPROM and then calling the bootloader. Thus, at the beginning of the bootloader, we have the following code:</p>


<pre>
  if (eeprom_read_word(START_MAIN_APP_ADDR) == 1 && IS_BIT_SET8(PINB, PB4)) {
    jump_to_main_program();
  }
</pre>

<p>If the flag is set and the button is up, we jump to the main program. Else, we stay in the bootloader and wait for a new firmware over sysex. Repressing the button will start the main software anyway (the following contraption waits for the button to go up, then down again):</p>

<pre>
    if (!IS_BIT_SET8(PINB, PB4)) {
      button = 1;
    } else {
      if (button) {
	jump_to_main_program();
      }
    }
</pre>


<p>There is one last thing we need to take care off, and that is checking if the firmware is valid. The flashing device sends a final checksum byte over MIDI that is the checksum of the whole firmware in memory. This checksum is written to the eeprom, and checked before jumping to the main program. If the checksum is correct, the firmware is valid. Else, we stay in the bootloader and wait for a new firmware:</p>

<pre>
uint8_t check_firmware_checksum(void) {
  uint16_t len = eeprom_read_word(FIRMWARE_LENGTH_ADDR);
  uint16_t firm_checksum = eeprom_read_word(FIRMWARE_CHECKSUM_ADDR);
  uint16_t i;
  uint16_t checksum = 0;
  
  for (i = 0; i < len; i++) {
    checksum += pgm_read_byte(i);
  }

  if ((checksum & 0x3FFF) == firm_checksum)
    return 1;
  else
    return 0;
}

uint8_t jump_to_main_program(void) {
  if (check_firmware_checksum()) {
    jump_to_app();
    return 1;
  } else {
    return 0;
  }
}
</pre>

<p>As you can see, the code for checksumming is very straightforware, and we just use pgm_read_byte to read from the flash memory. The length of the firmware is also stored in the eeprom.</p>

<p>That's how my MIDI bootloader works! :)</p>")


(post-to-blog *rw-blog* "AVR GUI Framework"
	      "<p>On the MIDI Command, I have 5 encoders that can work as push buttons as well, and an additional push button. I wanted to have a quick way to access GUI parameters (which button is pressed, which encoder was turned). I designed a small GUI framework to do that.</p>"

	      "<p>The rotary encoders send out a digital phase code using two wires, A and B. Depending on which line is pulled down first, one can access in which direction the encoder was turned. The push-button just provide a high value when up, and a low value when down. All these digital lines are connected to two 74hc165 which allow the atmel to read them out using a serial connection. The 165s are polled in a timer interrupt routine which runs at 500 hz, which sounds like a pretty slow polling, but it's actually largely enough for the quickest knob turns I manage to make. The result of polling the 165s is a 16 bit value that contains the status of the encoder A and Bs (5 * 2 bits) and the status of the 6 buttons (6 bits). The interrupt routine also updates a 16 bit counter called timer2_slowclock used to time things like flashing the display or how long a button can be pushed. The routines handle_buttons and handle_encoders are the actual framework and interpret the hardware data into something usable by my application code. The button handling code has to be quick and goes before clear_buttons. That's because some button events are edge-triggered (for example BUTTON_PRESSED or BUTTON_RELEASED). Most of it just sets flag that the main loop then interprets. Finally, the current button status is cleared to reset it for the next hardware polling.<p>

<pre>
ISR(TIMER2_OVF_vect) {
  timer2_slowclock++;

  uint8_t but = 0;
  uint16_t srenc  = 0;;
  static uint16_t srenc_old = 0;
  
  asm volatile(\"rcall sr165_read16\" \"\n\t\"
	       \"mov %0, r25\" \"\n\t\"
	       \"movw %A1, r24\" \"\n\t\"
	       : \"=r\" (but), \"=w\" (srenc) : : \"r24\", \"r25\");

  /* main button */
  handle_buttons(but);
  handle_encoders(srenc_old, srenc);
  srenc_old = srenc;

  /* button code comes here */

  clear_buttons();
}
</pre>

<p>The framework is based around two data structures that represent the current status of an encoder and the current status of a button.</p>

<pre>
typedef struct button_s {
  uint8_t status;
  uint16_t press_time;
  uint16_t last_press_time;
} button_t;

extern volatile button_t buttons[NUM_BUTTONS];

typedef struct encoder_s {
  int8_t normal;
  int8_t shift;
  int8_t button;
  int8_t button_shift;
} encoder_t;

extern volatile encoder_t encoders[NUM_ENCODERS];
</pre>

<p>The status of a button is represented as a bit field stored in the status variable. The flags for it are:</p>

<pre>
#define B_BIT_CURRENT      0
#define B_BIT_OLD          1
#define B_BIT_PRESSED_ONCE 2
#define B_BIT_DOUBLE_CLICK 3
#define B_BIT_CLICK        4
#define B_BIT_LONG_CLICK   5
</pre>

<p>I use a range of macros to access these fields (mostly I played with different memory representations of the button status to save on space.</p>

<pre>
#define BUTTON_DOWN(button)           (!(B_CURRENT(button)))
#define BUTTON_UP(button)             (B_CURRENT(button))
#define OLD_BUTTON_DOWN(button)       (!(B_OLD(button)))
#define OLD_BUTTON_UP(button)         (B_OLD(button))
#define BUTTON_PRESSED(button)        (OLD_BUTTON_UP(button) && BUTTON_DOWN(button))
#define BUTTON_DOUBLE_CLICKED(button) (B_DOUBLE_CLICK(button))
#define BUTTON_LONG_CLICKED(button)   (B_LONG_CLICK(button))
#define BUTTON_CLICKED(button)        (B_CLICK(button))
#define BUTTON_RELEASED(button)       (OLD_BUTTON_DOWN(button) && BUTTON_UP(button))
#define BUTTON_PRESS_TIME(button)     (clock_diff(B_PRESS_TIME(button), timer2_slowclock))

</pre>

<p>I access the button status in the following way (inside the interrupt routine, before clear_buttons):</p>

<pre>
if (BUTTON_PRESSED(0)) {
   set_flash_string(\"BUTTON 0\", \"PRESSED\");
}
</pre>

<p>It is important to not do anything CPU intensive in the button handling code. set_flash_string just copies a string into the flash-text buffer.</p>


<p>The encoder status is a relative number that is added to in each polling loop until is cleared by the main application. It can be both positive (right turn) or negative (left turn). Also, I differentiate between different button statuses while incrementing the encoder counters. Normal is when nothing is pressed and the encoder is turned. Shift is when the encoder is turned while the Shift button is pressed. Button is when the encoder is pressed and then turned, while button_shift is for when both Shift and the encoder itself are pressed.</p>

<p>The encoder data is accessed like this in the main routine:</p>

<pre>
static int16_t value = 0;
cli();
value += ENCODER_NORMAL(0);
clear_encoders();
sei();
</pre>

<p>The encoder handling code has to be executed while interrupts are disabled in order to not interfere with the hardware polling code. Thus it has to be quick as well. Most of the time the encoder values are read to update variables. Once all the encoders have been handled, clear_encoders is called to set all the encoder counters to 0.</p>

<p>Let's have a look at the actual hardware handling code (this is the C version, I actually use an optimised assembler version):</p>

<pre>
void handle_buttons(uint8_t but) {
  uint8_t but_tmp = but;
  uint8_t i;
  but_tmp >>= 2;
  for (i = 0; i < NUM_BUTTONS; i++) {
    STORE_B_CURRENT(i, IS_BIT_SET8(but_tmp, 0));

    if (BUTTON_PRESSED(i)) {
      B_PRESS_TIME(i) =  timer2_slowclock;

      if (B_PRESSED_ONCE(i)) {
	uint16_t diff = clock_diff(B_LAST_PRESS_TIME(i), B_PRESS_TIME(i));
	if (diff < DOUBLE_CLICK_TIME) {
	  SET_B_DOUBLE_CLICK(i);
	  CLEAR_B_PRESSED_ONCE(i);
	}
      } else {
	B_LAST_PRESS_TIME(i) = B_PRESS_TIME(i);
	SET_B_PRESSED_ONCE(i);
      }
    }

    if (BUTTON_DOWN(i) && B_PRESSED_ONCE(i)) {
      uint16_t diff = clock_diff(B_LAST_PRESS_TIME(i), timer2_slowclock);
      if (diff > LONG_CLICK_TIME) {
	SET_B_LONG_CLICK(i);
	CLEAR_B_PRESSED_ONCE(i);
      }
    }

    if (BUTTON_UP(i) && B_PRESSED_ONCE(i)) {
      uint16_t diff = clock_diff(B_LAST_PRESS_TIME(i), timer2_slowclock);
      if (diff > LONG_CLICK_TIME) {
	CLEAR_B_PRESSED_ONCE(i);
      } else if (diff > DOUBLE_CLICK_TIME) {
	CLEAR_B_PRESSED_ONCE(i);
	SET_B_CLICK(i);
      }
    }

    but_tmp >>= 1;
  }
}
</pre>

<p>This is not so complicated as it looks. We first skip the 2 encoder bits (remember, 10 bits of encoder data and 6 bits of button data). The for each button, we store the current status (if the button is up or down). If the button was pressed (that means it was up before and down now), we record the time it was pressed by reading timer2_slowclock. If it has already been pressed once, and the second press was a short time after the first press (shorter than DOUBLE_CLICK_TIME), we record a double click by clearing the press flag and setting the double click flag. Else, we record that this is the first press. </p>

<p>If the button is held down and has already been pressed, we check if the time of \"holding down\" is long enough to qualify as a long clck. If it is, we set the appropriate flag. If the button is released and was pressed once, we record it as a \"clicked\" event. The different between clicked and pressed is that you have to use clicked when you have both a short press and a long press function for the knob. We finally shift the button status and go on about handling the next button.</p>

<p>The encoder code is pretty similar in approach. We loop through the encoder bits, check if the knob was turned left or right, and record the offset according to the current button status. That's why handle_encoders is called after handle_buttons.</p>

<pre>
void handle_encoders(uint16_t srold_tmp, uint16_t sr_tmp) {
  uint8_t i;

  for (i = 0; i < NUM_ENCODERS; i++) {
    if (IS_BIT_SET8(sr_tmp, 0) == !IS_BIT_SET8(srold_tmp, 0)) {
      volatile int8_t *val = &(ENCODER_NORMAL(i));
      if (BUTTON_DOWN(i)) {
	if (BUTTON_DOWN(SHIFT_BUTTON))
	  val = &(ENCODER_BUTTON_SHIFT(i));
	else
	  val = &(ENCODER_BUTTON(i));
      } else if (BUTTON_DOWN(SHIFT_BUTTON)) {
	val = &(ENCODER_SHIFT(i));
      }

      if (IS_BIT_SET8(sr_tmp, 1) == IS_BIT_SET8(sr_tmp, 0)) {
	if (*val < 64)
	  (*val)++;
      } else {
	if (*val > -64)
	  (*val)--;
      }
    }
    sr_tmp >>= 2;
    srold_tmp >>= 2;
  }
}
</pre>

<p>For each encoder (2 bits of status), we check if there is an edge on the A wire (either positive or negative edge). We then update a pointer val according to the current pressed buttons, so that val points at either normal, shift, button or button_shift in the encoder structure. We then check if B is has changed to determine if it's a right turn or left turn. We then update the value, and make sure it doesn't go above 64 or below -64. We then shift the status, et voila :)</p>")

(post-to-blog *rw-blog* "Quick and Dirty Midi Merge under MacosX"
	      "<p>I needed some way to debug communications on my mac (sniff a MIDI connection). I tried to set up a MIDI Merge using MidiPipe, but sadly ran into problems while forwarding MIDI Sysex messages. So here is my quick and dirty workaround using the Carbon MIDI API. You can download the code here: http://bl0rg.net/~manuel/midi-merge.c .</p>"
	      "<p>It opens up to input ports and two output ports, and basically forwards everything coming in on the input ports to the output ports. When opening an input port, you can register a read procedure for every data coming in. This read procedure receives a list of packets containing the MIDI data (these packets have to be parsed further if you want to use the MIDI stream). However, I just copy the packets to a new packetlist, and send that list out of the appropriate output port. The code looks like this (starting with the main routine):</p>

<pre>
int main(int argc, char *argv[]) {
  int c;
  int outputDevice = -1;
  int inputDevice = -1;

  while ((c = getopt(argc, argv, \"hlb\")) != -1) {
    switch (c) {
    case 'l':
      listOutputMidiDevices();
      exit(0);
      break;

    case 'h':
    default:
      usage();
      exit(0);
      break;
    }
  }

  if ((optind + 2) != argc) {
    usage();
    exit(1);
  }
  outputDevice = atoi(argv[optind]);
  inputDevice = atoi(argv[optind+1]);

  if (outputDevice == -1 || inputDevice == -1) {
    usage();
    exit(1);
  }

  MIDIClientRef client = NULL;
  MIDIClientCreate(CFSTR(\"MIDI Send\"), NULL, NULL, &client);
  MIDIOutputPortCreate(client, CFSTR(\"Output port\"), &gOutPort);
  MIDIOutputPortCreate(client, CFSTR(\"Output port\"), &gOutPort2);
  gDest = MIDIGetDestination(outputDevice);
  gDest2 = MIDIGetDestination(inputDevice);
  MIDIPortRef inPort = NULL;
  MIDIInputPortCreate(client, CFSTR(\"Input port\"), myReadProc, NULL, &inPort);
  MIDIPortRef inPort2 = NULL;
  MIDIInputPortCreate(client, CFSTR(\"Input port\"), myReadProc2, NULL, &inPort2);
  

  MIDIEndpointRef src = MIDIGetSource(inputDevice);
  MIDIPortConnectSource(inPort, src, NULL);
  src = MIDIGetSource(outputDevice);
  MIDIPortConnectSource(inPort2, src, NULL);

  CFRunLoopRef runLoop;
  runLoop = CFRunLoopGetCurrent();

  CFRunLoopRun();

  return 0;
}
</pre>

<p>After the usual command-line argument parsing (including the option to list the available midi devices, which we'll get to shortly), we create a MIDI client (a way to register our application with the MIDI framework), and create two output ports. These output ports are used to send data to a destination. I don't really know one doesn't suffice, because the actual destination is stored in a separate variable. These variables are gDest and gDest2. We also create two input ports which are then connected to two sources. So the setup is: 2 destinations (gDest, gDest2) coupled to 2 output ports (gOutPort, gOutPort2), 2 sources coupled to 2 input ports (inPort, iNport2. These two input ports are registered to two read procedures (I was lazy here and didn't want to make some kind of structure to pass to the read procedure). Finally, we enter the normal Carbon Runloop.</p>

<p>The interesting code is thus in the read procedures (I'll show only one here):</p>
<pre>
void myReadProc(const MIDIPacketList *pktlist, void *refCon, void *connRefCon) {
  if (gOutPort != NULL && gDest != NULL) {
    MIDIPacket *packet = (MIDIPacket *)pktlist->packet;
    unsigned int j;
    int i;
    for (j = 0; j < pktlist->numPackets; j++) {
      midiSendPacket(packet, gOutPort, gDest);
      packet = MIDIPacketNext(packet);
    }
  }
}
</pre>

<p>We go through the received packets (in pktlist), and just resend them to the appropriate output port using the function midiSendPacket (which is a crude way to copy packet data into a new list and send it off):</p>

<pre>
void midiSendPacket(MIDIPacket *packet, MIDIPortRef outport, MIDIEndpointRef dest) {
  struct MIDIPacketList pktlist;
  pktlist.numPackets = 1;
  pktlist.packet[0].timeStamp = 0;
  pktlist.packet[0].length = packet->length;
  int i;
  for (i = 0; i < packet->length; i++) {
    pktlist.packet[0].data[i] = packet->data[i];
  }
  MIDISend(outport, dest, &pktlist); 
}
</pre>

<p>Finally, here is the code to show the available devices. I use one trick here, and that is to use the device name as the model name, and not the actual port's model. That's because I want to show the names I have assigned to the individual devices in Audio Midi Setup (I have like 4 interfaces called \"MidiLink\" connected most of the time).</p>

<pre>
void listOutputMidiDevices(void) {
  unsigned long   iNumDevs, i;
  
  /* Get the number of MIDI Out devices in this computer */
  iNumDevs = MIDIGetNumberOfDestinations();

  //  printf(\"%lu output midi devices found\r\n\", iNumDevs);
  
  /* Go through all of those devices, displaying their names */
  for (i = 0; i < iNumDevs; i++) {
    CFStringRef pname, pmanuf, pmodel;
    char name[64], manuf[64], model[64];
    
    MIDIEndpointRef ep = MIDIGetDestination(i);
    MIDIEntityRef ent;
    MIDIDeviceRef dev;
    MIDIEndpointGetEntity(ep, &ent);
    MIDIEntityGetDevice(ent, &dev);
    MIDIObjectGetStringProperty(ep, kMIDIPropertyName, &pname);
    MIDIObjectGetStringProperty(ep, kMIDIPropertyManufacturer, &pmanuf);
    MIDIObjectGetStringProperty(dev, kMIDIPropertyName, &pmodel);

    CFStringGetCString(pname, name, sizeof(name), 0);
    CFStringGetCString(pmanuf, manuf, sizeof(manuf), 0);
    CFStringGetCString(pmodel, model, sizeof(model), 0);
    CFRelease(pname);
    CFRelease(pmanuf);
    CFRelease(pmodel);
    
    printf(\"%d) %s - %s - %s\n\", i, name, manuf, model);
  }  
}

</pre>

<p>To compile the code, you have to link to the carbon framework. Here is the command-line I use:</p>

<pre>
gcc -framework CoreAudio -framework CoreMIDI -framework Carbon -o midi-merge midi-merge.c
</pre>

<p>Enjoy!</p>

")

(post-to-blog *rw-blog* "Wiimote Headtracking in Processing"
	      "bknr:/images/blog-wiimote-head.png

<p>We finally got to get something approaching headtracking in Processing, using a Wiimote and two cheap IR leds fixed to a bar (a kind of homebuilt IR-bar). We use the OPENGL mode of processing, and use darwiinremoteOSC and the libraries oscP5 and netP5 to obtain the IR data from darwiinremote. You can download the sketch here: http://bl0rg.net/~manuel/opengl2.pde .</p>"
	      "<p>The first part of the headtracking consists of getting the fx, fy and fz coordinates of the camera. The camera itself tracks 2 LEDs and gives us their X and Y coordinates in the FOV of the camera. These coordinates are already normalized by darwiinremote. We first need to isolate two points out of the data transmitted by the wiimote:</p>

<pre>
void ir(
float f10, float f11,float f12, 
float f20,float f21, float f22,
float f30, float f31, float f32,
float f40, float f41, float f42
) {
  ir[0] = f10;
  ir[1] = f11;
  ir[2] = f12;
  ir[3] = f20;
  ir[4] = f21;
  ir[5] = f22;
  ir[6] = f30;
  ir[7] = f31;
  ir[8] = f32;
  ir[9] = f40;
  ir[10] = f41;
  ir[11] = f42;

  points = 0;  
  for (int i = 0; i < 4; i += 3) {
    if (ir[i+2] < 15) {
        x[points] = 0.5 - ir[i];
        y[points] = 0.5 - ir[i+1];
      points++;
    } 
  }
}
</pre>

<p>The oscP5 code calls ir when new IR data is received, and gives us 12 parameters. Those are 4 times x, y, and brightness values for each tracked LED. If the brightness is bigger than 15, no point was recognized. We isolate the valid points, and use the two first values in x[] and y[] to calculate the distance of our viewer to the screen. The camera has a FOV of PI/4 (45 degrees). Thus the angle to the viewer is PI/4 and spans from -1 to 1. We store this value in the variable fov. We then calculate the distance dist between the two points. The bigger this distance, the nearer the viewer is to the screen. The angle from the camera to the bar is then fov / 2.0 * dist. Using this angle, we can then calculate the distance to the viewer relative to the width of the bar (in mm here, but all the exact calculation has not been done yet, it should be in pixel values).</p>

<pre>
  float fov = (PI / 4.0);
  float barWidth = 150.0; // bar width in mm
  if (points >= 2) {
    float dx = x[0] - x[1];
    float dy = y[0] - y[1];
    float dist = sqrt(dx * dx + dy * dy);

    float angle = fov * dist / 2.0;
    float headDist = (barWidth / 2.0) / tan(angle);
  }
</pre>

<p>Once we have the distance, we can calculate the real camera coordinates. I copied this formula out of Johnny Lee's code and have to admit I can't wrap my head around them at the moment (trigonometry burnout). We average the X and Y positions of the head bar, and then scale them to the real x and y using the distance and the fov. We update the camera position taking into account the axis of the wiimote (empirically determined, we put the wiimote with buttons down on the table, we don't have a stand yet):</p>

<pre>
    float rx = sin(fov * mx) * headDist * 0.5;
    float ry = sin(fov * my) * headDist * 1.5;
    fx = -rx;
    fy = -ry;
    fz = headDist;
</pre>

<p>This is the calculation of the camera position. We now need to update our opengl view according to the position. We have to update both the camera position and the frustum of the camera (because we are moving relative to the actual viewing screen.</p>

<p>First we move the camera to fx, fy, fz and make it look straight ahead:</p>

<pre>
  camera(fx, fy, fz,
         fx, fy, 0,
         0, 1, 0);
</pre>

<p>We set the near plane at fz. We then shift the frustum coordinates according to our position. The normal frustum if we are at 0, 0, viewing_distance should be -width/2, width/2, -height/2, height/2. </p>

<pre>
  float near = fz;
  float left, right, top, bottom;
  float angle = radians(60.0);
  float facd = (width/2.0) / tan(angle / 2.0);
  near = 20.0;
  left = -(width/2.0) + fx;
  right = (width/2.0) + fx;
  top = -(height/2.0) + fy;
  bottom = (height/2.0) + fy;
</pre>

<p>Viewing_distance is empirically determined to be width/2.0, and our FOV (the viewers FOV) to be 60 degrees. We scale the frustum values by viewing_distance / tan(FOV/2) to have the right perspective. </p>

<pre>
  left /= facd / fz;
  right /= facd / fz;
  top /= facd / fz;
  bottom /= facd / fz;
</pre>

<p>Finally, we scale the whole view to have our the coordinates on our near plane:</p>

<pre>
  left *= near / fz;
  right *= near / fz;
  top *= near / fz;
  bottom *= near / fz;
  frustum(left, right, top, bottom, near, 60000);
</pre>

<p>We can then call our main drawing routine which will draw a tunnel and a few targets to make for a real environment.</p>

<p>This is still very beta and I haven't understood all the issues in the code yet, but it looks cool and it's fun to play with :)</p>").