-- {-# LANGUAGE ExistentialQuantification, RankNTypes, CPP, BangPatterns #-}
-- | Word32 or Word64 - depending on implementation. Format just specifies "uint".
--   4-Byte in the documentation indicates Word32 - but not specified!
module Importer.IQM.Types where

import Control.Monad.Trans.State.Lazy (StateT)
import Data.Int
import Data.Word
import Data.ByteString
import Data.Attoparsec.ByteString.Char8
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Types
import Prelude as P
import Foreign.Storable
import Foreign.C.Types
import Graphics.Rendering.OpenGL.GL.BufferObjects hiding (Offset)
import Graphics.Rendering.OpenGL.GL.VertexArrayObjects

-- | Mesh-Indices to distinguish the meshes referenced
newtype Mesh = Mesh Word32 deriving (Show, Eq)
-- | State-Wrapped Parser-Monad which is capable of counting the
--   Bytes read for offset-gap reasons
type CParser a = StateT Int64 Parser a

-- | Alias
type Flags = GLbitfield      -- ^ Alias for UInt32

-- | Alias
type Offset = Word32         -- ^ Alias for UInt32

-- | Alias
type Index = GLuint          -- ^ Alias for UInt32

-- | Alias
type NumComponents = GLsizei -- ^ Alias for UInt32

-- | Data-BLOB inside IQM
type IQMData = Ptr IQMVertexArrayFormat -- ^ Pointer for Data

-- | Header of IQM-Format.
--
--   ofs_* fields are relative to the beginning of the iqmheader struct
--
--   ofs_* fields are set to 0 when data is empty
--
--   ofs_* fields are aligned at 4-byte-boundaries
data IQMHeader = IQMHeader
                { version            :: !Word32 -- ^ Must be 2
                , filesize           :: !Word32
                , flags              :: !Flags
                , num_text           :: !Word32
                , ofs_text           :: !Offset
                , num_meshes         :: !Word32
                , ofs_meshes         :: !Offset
                , num_vertexarrays   :: !Word32
                , num_vertexes       :: !Word32
                , ofs_vertexarrays   :: !Offset
                , num_triangles      :: !Word32
                , ofs_triangles      :: !Offset
                , ofs_adjacency      :: !Offset
                , num_joints         :: !Word32
                , ofs_joints         :: !Offset
                , num_poses          :: !Word32
                , ofs_poses          :: !Offset
                , num_anims          :: !Word32
                , ofs_anims          :: !Offset
                , num_frames         :: !Word32
                , num_framechannels  :: !Word32
                , ofs_frames         :: !Offset
                , ofs_bounds         :: !Offset
                , num_comment        :: !Word32
                , ofs_comment        :: !Offset
                , num_extensions     :: !Word32 -- ^ stored as linked list, not as array.
                , ofs_extensions     :: !Offset
                } deriving (Show, Eq)

-- | Format of an IQM-Mesh Structure.
--
--   Read it like a Header of the Meshes lateron in the Format
data IQMMesh = IQMMesh
                { meshName              :: Maybe Mesh
                , meshMaterial          :: Word32
                , meshFirstVertex       :: Word32
                , meshNumVertexes       :: Word32
                , meshFirstTriangle     :: Word32
                , meshNumTriangles      :: Word32
                } deriving (Show, Eq)

-- | Format of IQM-Triangle Structure
data IQMTriangle = IQMTriangle VertexIndex VertexIndex VertexIndex

-- | Type-Alias for Word32 indicating an index on vertices in IQMMesh
type VertexIndex = Word32

-- | Type-Alias for Word32 indicating an index on IQMTriangle
type TriangleIndex = Word32

-- | From the IQM-Format-Description:
--
--   each value is the index of the adjacent triangle for edge 0, 1, and 2, where ~0 (= -1)
--   indicates no adjacent triangle indexes are relative to the iqmheader.ofs_triangles array
--   and span all meshes, where 0 is the first triangle, 1 is the second, 2 is the third, etc.
data IQMAdjacency = IQMAdjacency TriangleIndex TriangleIndex TriangleIndex

-- | Format of a whole IQM-File
--
--   still unfinished!
data IQM = IQM
        { header                :: IQMHeader
        , texts                 :: [ByteString]
        , meshes                :: [IQMMesh]
        , vertexArrays          :: [IQMVertexArray]
        , vertexBufferObjects   :: [BufferObject]
        , vertexArrayObject     :: VertexArrayObject
        , triangles             :: Ptr Word32
        } deriving (Show, Eq)

-- | Different Vertex-Array-Types in IQM
--
--   Custom Types have to be > 0x10 as of specification
data IQMVertexArrayType = IQMPosition
                       | IQMTexCoord
                       | IQMNormal
                       | IQMTangent
                       | IQMBlendIndexes
                       | IQMBlendWeights
                       | IQMColor
                       | IQMCustomVAT Word32
                       deriving (Show, Eq)

-- | Lookup-Function for internal enum to VertexArrayFormat
rawEnumToVAT :: Word32 -> CParser IQMVertexArrayType
rawEnumToVAT 0 = return IQMPosition
rawEnumToVAT 1 = return IQMTexCoord
rawEnumToVAT 2 = return IQMNormal
rawEnumToVAT 3 = return IQMTangent
rawEnumToVAT 4 = return IQMBlendIndexes
rawEnumToVAT 5 = return IQMBlendWeights
rawEnumToVAT 6 = return IQMColor
rawEnumToVAT a = return $ IQMCustomVAT a

-- | Vetrex-Array-Format of the data found at offset
data IQMVertexArrayFormat = IQMbyte
                            | IQMubyte
                            | IQMshort
                            | IQMushort
                            | IQMint
                            | IQMuint
                            | IQMhalf
                            | IQMfloat
                            | IQMdouble
--                            | Unknown Word32
                       deriving (Show, Eq)

-- | Get the Size (in Bytes) of the given IQMVertexArrayFormat-Struct
vaSize :: IQMVertexArrayFormat -> Int
vaSize IQMbyte 		= sizeOf (undefined :: CSChar)
vaSize IQMubyte 	= sizeOf (undefined :: CUChar)
vaSize IQMshort 	= sizeOf (undefined :: CShort)
vaSize IQMushort 	= sizeOf (undefined :: CUShort)
vaSize IQMint 		= sizeOf (undefined :: CInt)
vaSize IQMuint 		= sizeOf (undefined :: CUInt)
vaSize IQMhalf 		= sizeOf (undefined :: Word16) --TODO: Find 16-Bit-Float-Datatype FIXME!
vaSize IQMfloat 	= sizeOf (undefined :: CFloat)
vaSize IQMdouble 	= sizeOf (undefined :: CDouble)

--mallocVArray :: Storable a => IQMVertexArrayFormat -> Int -> IO (Ptr a)
--mallocVArray IQMbyte n	= mallocArray n :: IO (Ptr CSChar)
--mallocVArray IQMubyte n	= mallocArray n :: IO (Ptr CUChar)

-- | Lookup-Function for internal enum to VertexArrayFormat
rawEnumToVAF :: Word32 -> CParser IQMVertexArrayFormat
rawEnumToVAF 0 = return IQMbyte
rawEnumToVAF 1 = return IQMubyte
rawEnumToVAF 2 = return IQMshort
rawEnumToVAF 3 = return IQMushort
rawEnumToVAF 4 = return IQMint
rawEnumToVAF 5 = return IQMuint
rawEnumToVAF 6 = return IQMhalf
rawEnumToVAF 7 = return IQMfloat
rawEnumToVAF 8 = return IQMdouble
--rawEnumToVAF a = return $ Unknown a
rawEnumToVAF a = fail $ P.concat ["unrecognized enum(",show a,") in VertexArrayFormat"]


-- | A Vertex-Array-Definiton.
--
--   The Vertex starts at Offset and has num_vertexes * NumComponents entries.
--
--   All Vertex-Arrays seem to have the same number of components, just differ in Type, Format
--   and Flags
data IQMVertexArray = IQMVertexArray
                        IQMVertexArrayType
                        Flags
                        IQMVertexArrayFormat
                        NumComponents
                        Offset
                        IQMData
                       deriving (Eq)
instance Show IQMVertexArray where
    show (IQMVertexArray t fl fo nc off dat) = "IQMVertexArray (Type: " ++ show t ++
                                                        ", Flags: " ++ show fl ++
                                                        ", Format: " ++ show fo ++
                                                        ", NumComponents: " ++ show nc ++
                                                        ", Offset: " ++ show off ++
							", Data at: " ++ show dat ++
                                                        ")"

