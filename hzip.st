#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Written by Morgan Grant  (mlgrant@ucsc.edu)"
"       and Ankur Goswami (agoswam3 @ucsc.edu)"

FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^bit.
   ]

   readByte [
      |result|
      result := 0.
      8 to: 1 by: -1 do: [:i|
         |bit|
         bit := self nextBit.
         result := result bitAt: i put: bit.
      ].
      ^ result.
   ]

   resetStream [
      self position: 0.
      bitIndex := 0.
   ]
]

SmallInteger extend [
   isPrint [
      ^ (Character space value <= self) & (self <= $~ value)
   ]
   asCharFmt [
      self isPrint ifTrue: [^ ' $', (Character value: self) asString]
                   ifFalse: [^ self asFixedWidth: 3]
   ]
   asFixedWidth: width [
      | return |
      self asString size >= width ifTrue: [^ self asString].
      return := ''.
      1 to: width - (self asString size) do: [:i| 
         return := return, ' '.
      ].
      ^ return, self asString.
   ]
]

Object subclass: Stack [
   |theArray size|

   Stack class >> new [
      |result|
      result := super new.
      result init: 257.
      ^ result.
   ]

   init: n [
      theArray := Array new: n.
      size := 0.
   ]

   push: item [
      size := self size + 1.
      theArray at: size put: item.
   ]

   pop [
      |item|
      item := theArray at: size.
      size := self size - 1.
      ^ item.
   ]

   size [^size]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]

   writeBitString: string [
      string do: [:char|
         char = $0 ifTrue: [
            self writeBit: 0.
         ] ifFalse: [
            self writeBit: 1.
         ]
      ]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count) | 
      ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: outwriter [
      outwriter writeBit: 0.
      8 to: 1 by: -1 do: [:i|
         outwriter writeBit: (self char bitAt: i)
      ].
      self char = 256 ifTrue: [outwriter writeBit: 1].
      self char = 0 ifTrue: [outwriter writeBit: 0].
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: outwriter [
      left postorder: outwriter.
      right postorder: outwriter.
      outwriter writeBit: 1.
   ]

   left [^left]
   right [^right]
]

Object subclass: Encoder [
   | root freqtable encodestrings |

   Encoder class >> new [
      self shouldNotImplement
   ]

   Encoder class >> new: inFile [
      |result|
      result := super new.
      result init: inFile.
      ^ result.
   ]

   init: inFile [
      "builds freqtable,  decode tree and encoding strings set"
      |priority|

      freqtable := ZeroArray new: 257.
      [inFile atEnd not] whileTrue: [
         |ordchar|
         ordchar := inFile next asInteger.
         freqtable incr: ordchar.
      ].
      inFile resetStream.
      freqtable incr: 256.

      priority := SortedCollection new.
      freqtable keysAndValuesDo: [:char :freq|
         (freq isNil not and: [freq > 0]) ifTrue: [
            priority add: (Leaf new: char count: freq).
         ]
      ].
      
      [priority size > 1] whileTrue: [
         |left right newTree|
         left := priority removeFirst.
         right := priority removeFirst.

         newTree := Tree new: (left char) 
                         count: (left count + right count) 
                         left: left right: right.
         priority add: newTree.
      ].
      root := priority first.

      encodestrings := ZeroArray new: 257.
      root inorder: [:char :string|
         encodestrings at: char put: string.
      ] prefix: ''.
   ]

   writeEncodeTree: outwriter [
      root postorder: outwriter.
      outwriter  writeBit: 1.
   ]

   writeEncodedFile: outwriter file: inFile [
      [inFile atEnd not] whileTrue: [
         |ordchar encodestring|
         ordchar := inFile next asInteger.
         encodestring := self encodestrings at: ordchar.
         outwriter writeBitString: encodestring.
      ].
      outwriter writeBitString: (self encodestrings at: 256).
   ]

   root [^root]
   freqtable [^freqtable]
   encodestrings [^encodestrings]
]

Object subclass: Decoder [
   |sourcefile root|

   Decoder class >> new [
      self shouldNotImplement
   ]

   Decoder class >> new: infile [
      |result|
      result := super new.
      result init: infile.
      ^ result.
   ]

   init: infile [
      |stack done|
      sourcefile := infile.
      stack := Stack new.

      done := false.
      [done not] whileTrue: [
         |bit|
         bit := infile nextBit.

         bit = 0 ifTrue: [
            |byte leaf|
            byte := infile readByte.
            byte = 0 ifTrue: [
               infile nextBit = 1 ifTrue: [byte := 256].
            ].
            leaf := Leaf new: byte count: 0.
            stack push: leaf.
         ] ifFalse: [
            stack size = 1 ifTrue: [
               done := true.
            ] ifFalse: [
               |left right inner|
               right := stack pop.
               left := stack pop.
               inner := Tree new: left char count: 0 left: left 
                                                     right: right.
               stack push: inner.
            ].
         ].
      ].
      root := stack pop.
   ]

   writeDecodedFile: outfile [
      |done curnode|

      done := false.
      curnode := self root.
      [done not] whileTrue: [
         |bit|
         bit := self sourcefile nextBit.
         
         bit = 0 ifTrue: [
            curnode := curnode left.
         ] ifFalse: [
            curnode := curnode right.
         ].

         (curnode isMemberOf: Leaf) ifTrue: [
            curnode char = 256 ifTrue: [
               done := true.
            ] ifFalse: [
               outfile nextPutByte: curnode char.
            ].
            curnode := self root.
         ].
      ]
   ]

   sourcefile [^sourcefile]
   root [^root]
]

"begin actual program"
nl := Character nl.

Smalltalk arguments size < 2 ifTrue: [
   stderr << 'must specify at least two arguments' << nl.
   ObjectMemory quit: 1.
].

|opt infile outfile|

opt := Smalltalk arguments at: 1.
infile := FileStream open: (Smalltalk arguments at: 2)
                     mode: FileStream read.

Smalltalk arguments size >= 3 ifTrue: [
   outfile := FileStream open: (Smalltalk arguments at: 3)
                         mode: FileStream write.
] ifFalse: [
   outfile := stdout.
].

"apparently smalltalk has no switch statement"
opt = '-d' ifTrue: [

   "if debug mode is chosen"
   stdout << 'ya chose debug mode! nice!' << nl.

] ifFalse: [
opt = '-t' ifTrue: [
   "if tree mode is chosen"
   
   encode := Encoder new: infile.
   encode encodestrings keysAndValuesDo: [:char :string|
      string isNil not ifTrue: [
         stdout << char asCharFmt << ' '
                << ((encode freqtable at: char) asFixedWidth: 7) << ' '
                << string << nl.
      ]
   ].

] ifFalse: [
opt = '-c' ifTrue: [
   "if compress mode is chosen"

   encode := Encoder new: infile.
   outwriter := OutBits new: outfile.
   encode writeEncodeTree: outwriter.
   encode writeEncodedFile: outwriter file: infile.
   outwriter flushByte.

] ifFalse: [
opt = '-u' ifTrue: [
   "if uncompress mode is chosen"

   decode := Decoder new: infile.
   decode writeDecodedFile: outfile.

] ifFalse: [
   "invalid mode given"
   stderr << 'invalid mode: ' << opt << nl.
]]]].


infile close.
(outfile = stdout) not ifTrue: [
   outfile close.
].
