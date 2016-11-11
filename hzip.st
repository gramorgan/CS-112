#!/usr/bin/gst -f
"#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f"

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
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

   resetStream [
      self position: 0.
      bitIndex := 0.
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
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

]

Object subclass: DecodeTree [
   | root |

   DecodeTree class >> new [
      self shouldNotImplement
   ]

   DecodeTree class >> new: inFile [
      | result |
      result := super new.
      result init: inFile.
      ^ result.
   ]

   init: inFile [
      | freqtable priority |

      freqtable := ZeroArray new: 256.
      [inFile atEnd not] whileTrue: [
         | ordchar |
         ordchar := inFile next asInteger.
         freqtable incr: ordchar.
      ].

      priority := SortedCollection new.
      freqtable keysAndValuesDo: [:char :freq|
         (freq isNil not and: [freq > 0]) ifTrue: [
            priority add: (Leaf new: char count: freq).
         ]
      ].
      
      [priority size > 1] whileTrue: [
         | left right newTree |
         left := priority removeFirst.
         right := priority removeFirst.

         newTree := Tree new: (left char) count: (left count + right count) left: left right: right.
         priority add newTree.
      ].
      self setRoot: priority first.

      stdout << self root << nl.
   ]

   setRoot: newRoot [
      root := newRoot.
   ]

   root [^ root]
]

nl := Character nl.

Smalltalk arguments size < 2 ifTrue: [
   stderr << 'must specify at least two arguments' << nl.
   ObjectMemory quit: 1.
].

| opt infile outfile |

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
   stdout << 'd' << nl.
] ifFalse: [
opt = '-t' ifTrue: [
   "if tree mode is chosen"
   
   DecodeTree new: infile.
] ifFalse: [
opt = '-c' ifTrue: [
   "if compress mode is chosen"
   stdout << 'c' << nl.
] ifFalse: [
opt = '-u' ifTrue: [
   "if uncompress mode is chosen"
   stdout << 'u' << nl.
] ifFalse: [
   "invalid mode given"
   stderr << 'invalid mode: ' << opt << nl.
   ObjectMemory quit: 1.
]]]].
