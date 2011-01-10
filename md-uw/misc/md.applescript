-- PLEASE SCROLL DOWN TO EDIT channel_machines AND rom_pitches
-- CONTACT manuel@bl0rg.net FOR BUG REPORTS AND INFO

property efm_rs : 1
property efm_hh : 2
property efm_cp : 3
property efm_sd : 4
property efm_xt : 5
property efm_bd : 6
property trx_cl : 7
property trx_sn : 8
property trx_xc : 9
property trx_xt : 10
property trx_bd : 11
property rom : 12
property none : -1

-- PLEASE EDIT THE FOLLOWING LINES. channel_machines CONTAINS THE 
-- NAME OF THE MACHINES USED ON EACH TRACK. SO FOR EXAMPLE THE 
-- LINE BELOW SHOWS THAT TRACK 1 IS AN EFM : BD, TRACK 2 AN EFM HH, 
-- TRACK 3 AN EFM SD, TRACK 4 A ROM MACHINE, TRACK 5 ANOTHER EFM HH

property channel_machines : {efm_bd, efm_hh, efm_sd, rom, efm_hh, none, none, none, none, none, none, none, none, none, none, none}

-- THE FOLLOWING LINE CONTAINS THE BASE PITCH OF THE SAMPLE STORED -- IN THE ROM MACHINE, SO FOR EXAMPLE THE SAMPLE ON TRACK 4 HAS A 
-- BASE PITCH OF 60
property rom_pitches: { 0, 0, 0, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

property track_channels : {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}
property track_notes : {36, 38, 40, 41, 43, 45, 47, 48, 50, 52, 53, 55, 57, 58, 60, 62}
property track_ccs : {16, 40, 72, 96, 16, 40, 72, 96, 16, 40, 72, 96, 16, 40, 72, 96}
 
property efm_rs_notes : {1, 3, 6, 9, 11, 14, 17, 19, 22, 25, 27, 30, 33, 35, 38, 41, 43, 46, 49, 51, 54, 57, 59, 62, 65, 67, 70, 73, 75, 78, 81, 83, 86, 89, 91, 94, 97, 99, 102, 105, 107, 110, 113, 115, 118, 121, 123, 126}
property efm_hh_notes : {1, 5, 9, 14, 18, 22, 27, 31, 35, 39, 44, 48, 52, 56, 61, 65, 69, 73, 78, 82, 86, 91, 95, 99, 103, 108, 112, 116, 120, 125}
property efm_cp_notes : {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125, 127}
property efm_sd_notes : {1, 5, 9, 14, 18, 22, 27, 31, 35, 39, 44, 48, 52, 56, 61, 65, 69, 73, 78, 82, 86, 91, 95, 99, 103, 108, 112, 116, 120, 125}
property efm_xt_notes : {1, 7, 12, 17, 23, 28, 33, 39, 44, 49, 55, 60, 65, 71, 76, 81, 87, 92, 97, 102, 108, 113, 118, 124}
property efm_bd_notes : {1, 3, 6, 9, 11, 14, 17, 19, 22, 25, 27, 30, 33, 35, 38, 41, 43, 46, 49, 51, 54, 57, 59, 62, 65, 67, 70, 73, 75, 78, 81, 83, 86, 89, 91, 94, 97, 99, 102, 105, 107, 110, 113, 115, 118, 121, 123, 126}
property trx_cl_notes : {5, 11, 17, 23, 29, 36, 42, 48, 54, 60, 66, 72, 78, 84, 91, 97, 103, 109, 115, 121, 127}
property trx_sn_notes : {3, 13, 24, 35, 45, 56, 67, 77, 88, 98, 109, 120}
property trx_xc_notes : {1, 6, 11, 17, 22, 27, 33, 38, 43, 49, 54, 60, 65, 70, 76, 81, 86, 92, 97, 102, 108, 113, 118, 124}
property trx_xt_notes : {2, 7, 12, 18, 23, 28, 34, 39, 44, 49, 55, 60, 65, 71, 76, 81, 87, 92, 97, 103, 108, 113, 118, 124}
property trx_bd_notes : {1, 7, 12, 17, 23, 28, 33, 39, 44, 49, 55, 60, 66, 71, 76, 82, 87, 92, 98, 103, 108, 114, 119, 124}
property rom_notes : {0, 2, 5, 7, 9, 12, 14, 16, 19, 21, 23, 26, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79, 82, 85, 88, 91, 94, 97, 100, 102, 105, 107, 109, 112, 114, 116, 119, 121, 123, 126}
property md_mappings : {efm_rs:{basenote:71, pitch:efm_rs_notes}, efm_hh:{basenote:71, pitch:efm_hh_notes}, efm_cp:{basenote:59, pitch:efm_cp_notes}, efm_sd:{basenote:59, pitch:efm_sd_notes}, efm_xt:{basenote:41, pitch:efm_xt_notes}, efm_bd:{basenote:32, pitch:efm_bd_notes}, trx_cl:{basenote:95, pitch:trx_cl_notes}, trx_sn:{basenote:75, pitch:trx_sn_notes}, trx_xc:{basenote:53, pitch:trx_xc_notes}, trx_xt:{basenote:40, pitch:trx_xt_notes}, trx_bd:{basenote:35, pitch:trx_bd_notes}, rom:{basenote:-1, pitch:rom_notes}}
property md_mappings_array : {efm_rs of md_mappings, efm_hh of md_mappings, efm_cp of md_mappings, efm_sd of md_mappings, efm_xt of md_mappings, efm_bd of md_mappings, trx_cl of md_mappings, trx_sn of md_mappings, trx_xc of md_mappings, trx_xt of md_mappings, trx_bd of md_mappings, rom of md_mappings}

on runme(message)
	set status to item 1 of message
         if (status >= 128) and (status < 128 + 16) then
            return
         end if
	if (status ≥ 144) and (status < 144 + 16) then
		set track to status - 144 + 1
		set md_machine to item track of channel_machines
		if (md_machine = none) then
			return
		end if
		set md_note to item track of track_notes

		set channel to item track of track_channels
		set cc to item track of track_ccs
		set mapping to item md_machine of md_mappings_array
		set basenote to basenote of mapping
		set pitch to pitch of mapping
		
		set mynote to item 2 of message
		set mynote to mynote - basenote + 1
		if (basenote = -1) then
                            set rom_pitch to item track of rom_pitches
			set mynote to mynote - rom_pitch + 23
		end if
-- say mynote		
		if (mynote > 0) then
			if (item 3 of message = 0) then
				set item 1 of message to 176 + channel
				set item 2 of message to cc
				set item 3 of message to item mynote of pitch
				if (mynote > 0) then
					return message
				end if
			else
                                     set item 1 of message to 144
				set item 2 of message to md_note
				return message
			end if
		end if
	else
		return message
	end if
end runme
