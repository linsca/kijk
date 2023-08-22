#       Kijk - A plugin to visualize EMA (position) data directly in praat
#
#   Author: Lins Machado, C.
#   Correspondence: carolina@machado.eti.br
#
# praat version >= 6.2.13
#
# kijk version 0.2
#

clearinfo


# 1- files must be selected before running the script
table$ = selected$()


# 2- Ask the user: sensor number/name, axis, type of EMA device (later)
form Kijk - visualize EMA trajectories
    comment Indicate the sensor number, name, and axis you wish to view
    sentence Sensor_number 3 5
    sentence Sensor_name TD TB
    sentence Dimension X Y
    boolean Custom_table 0
    choice Device: 1
        button NDI wave
        button AG501
    integer Sampling_frequency 400
    boolean Combine_sensors 0
endform


# 3- Variables collected 
sensors$# = splitByWhitespace$# (sensor_number$)
names$# = splitByWhitespace$# (sensor_name$)
axis$# = splitByWhitespace$# (dimension$)
regexAxis$ = "["+replace$(dimension$, " ", "", 0)+"]"


# 4- Check device used and if data input has custom headers
if device = 1
    if custom_table = 1
        @dataToExtract: table$
    else
        @processNDI: table$
        @dataToExtract: table$
    endif
 else
    @dataToExtract: table$
endif


# 5- Find and output table of sensors found (if device = NDI Wave)
procedure processNDI: .inTable$
    refTable = Create Table with column names: "refTable", 0, "sensor_label sensor_num col_number"
    selectObject: .inTable$
        nCol = Get number of columns
        .appender = 0
        for i to nCol
            selectObject: .inTable$
                colLabel$ = Get column label: i
                if index_regex(colLabel$, "(?isensor \d* id)")
                    num_sensor$# = splitByWhitespace$# (colLabel$)
                    .appender += 1
                    selectObject: refTable
                        Append row
                        Set string value: .appender, "sensor_label", colLabel$
                        Set string value: .appender, "sensor_num", num_sensor$#[2]
                        Set numeric value: .appender, "col_number", i
                endif                
        endfor
        
    # Rename var names adding sensor number to col labels
    selectObject: refTable
        nRow = Get number of rows
        for n to nRow
            selectObject: refTable
                currentSensor$ = Get value: n, "sensor_label"
                num$ = Get value: n, "sensor_num" 
                    for j to nCol
                        selectObject: .inTable$
                            cLabel$ = Get column label: j
                            if cLabel$ = currentSensor$
                                col = j
                                for c from col to col+8
                                    selectObject: .inTable$
                                        oldLabel$ = Get column label: c
                                        if oldLabel$ <> currentSensor$
                                        Set column label (index): c, oldLabel$+"_"+num$
                                        endif
                                endfor
                            endif

                    endfor

        endfor

    removeObject: refTable 
   
endproc


# 6- Sensors and axes to be extracted
procedure dataToExtract: .inTable$
    .refNames$# = empty$#(size(names$#))
     # appendInfoLine: size(names$#) ; = 1

    selectObject: .inTable$
         if device != 2
            colLabel$ = Get column label: 1
            if index_regex(colLabel$, "(?itime)")
                .emaStart = Get value: 1, colLabel$ ; starting time for NDI device
            endif
        endif
        traponsedTable = Transpose
        Set column label (index): 1, "col_names"

        for i to size(names$#)
            selectObject: traponsedTable
                if custom_table = 1
                    sensorTable = Extract rows where column (text): "col_names", "contains", names$#[i]
                    nameEMA$ = "EMA_Sensor_"+names$#[i]
                else
                    if device = 1
                        sensorTable = Extract rows where column (text): "col_names", "contains", sensors$#[i]
                    else
                        sensorID$ = "Ch"+sensors$#[i]+"_"
                        sensorTable = Extract rows where column (text): "col_names", "contains", sensorID$
                    endif                 
                    nameEMA$ = "EMA_Sensor_"+sensors$#[i]+"_"+names$#[i]
                endif
                
                Rename: nameEMA$
                .refNames$# [i] = nameEMA$

                nRow = Get number of rows
                for n to nRow
                    backN = nRow - n + 1
                    val$ = Get value: backN, "col_names"
                    if index_regex(val$, regexAxis$) ;select only desired dimensions
                    else
                        selectObject: sensorTable
                        Remove row: backN
                    endif
                endfor
                
            @toSignal: sensorTable

        endfor
     removeObject: traponsedTable
endproc


# 7- Convert table to signal containing trajectories
procedure toSignal: .inTable

    selectObject: .inTable
        matrix = Down to Matrix
        emaChannels = To Sound
        emaName$ = selected$("Sound")
        Override sampling frequency: sampling_frequency
            # get starting time of wave and ema element
                if device = 1
                    if dataToExtract.emaStart != 0
                        Shift times to: "start time", dataToExtract.emaStart
                    endif
                endif            

        .emaFinal$ = selected$("Sound")
        
    if combine_sensors = 0
        @infoOutput: .emaFinal$
    endif

    removeObject: matrix, .inTable
endproc


# 8- Combine sensors in single signal
if combine_sensors = 1
    for i to size(dataToExtract.refNames$#)
        plusObject: "Sound "+dataToExtract.refNames$#[i]
    endfor
    Combine to stereo
    Rename: "EMA_Sensors_Combined"
    combined$ = selected$()
    @infoOutput: combined$
    minusObject: combined$
    for i to size(dataToExtract.refNames$#)
        plusObject: "Sound "+dataToExtract.refNames$#[i]
        Remove
    endfor
endif


# 9- Output channel correspondences  
procedure infoOutput: .inSound$
    if combine_sensors = 0
        appendInfoLine: "Summary of ", .inSound$
        for .a to size(axis$#)
            appendInfoLine: "Ch ", .a,": ", axis$#[.a]
        endfor
        appendInfoLine: ""
    else
        appendInfoLine: "Summary of ", .inSound$
        appendInfoLine: ""
        .incrementer = 1
        for .i to size(names$#)
            if custom_table = 1
                appendInfoLine: "Sensor: ", names$#[.i]
                for .j to size(axis$#)
                     appendInfoLine: "Ch ", .incrementer, ": ", axis$#[.j]
                     .incrementer +=1
                endfor
            else
                appendInfoLine: "Sensor: ", sensors$#[.i], " (", names$#[.i], ")"
                for .j to size(axis$#)
                    appendInfoLine: "Ch ", .incrementer, ": ", names$#[.i], "_", axis$#[.j]
                    .incrementer +=1
                 endfor
            endif
        endfor      
    endif
endproc