-- | Int32 or Int64 - depending on implementation. Format just specifies "uint".
--   4-Byte in the documentation indicates Int32 - but not specified!
module Importer.IQM.Types where

import Data.Int
import Data.ByteString
import Data.Attoparsec.ByteString.Char8
import Control.Monad.Trans.State.Lazy (StateT)

-- | Mesh-Indices to distinguish the meshes referenced
newtype Mesh = Mesh Int32 deriving (Show, Eq)
-- | State-Wrapped Parser-Monad which is capable of counting the
--   Bytes read for offset-gap reasons
type CParser a = StateT Int64 Parser a



-- | Header of IQM-Format.
--
--   ofs_* fields are relative to the beginning of the iqmheader struct
--
--   ofs_* fields are set to 0 when data is empty
--
--   ofs_* fields are aligned at 4-byte-boundaries
data IQMHeader = IQMHeader
                { version            :: Int32 -- ^ Must be 2
                , filesize           :: Int32
                , flags              :: Int32
                , num_text           :: Int32
                , ofs_text           :: Int32
                , num_meshes         :: Int32
                , ofs_meshes         :: Int32
                , num_vertexarrays   :: Int32
                , num_vertexes       :: Int32
                , ofs_vertexarrays   :: Int32
                , num_triangles      :: Int32
                , ofs_triangles      :: Int32
                , ofs_adjacency      :: Int32
                , num_joints         :: Int32
                , ofs_joints         :: Int32
                , num_poses          :: Int32
                , ofs_poses          :: Int32
                , num_anims          :: Int32
                , ofs_anims          :: Int32
                , num_frames         :: Int32
                , num_framechannels  :: Int32
                , ofs_frames         :: Int32
                , ofs_bounds         :: Int32
                , num_comment        :: Int32
                , ofs_comment        :: Int32
                , num_extensions     :: Int32 -- ^ stored as linked list, not as array.
                , ofs_extensions     :: Int32
                } deriving (Show, Eq)

-- | Format of an IQM-Mesh Structure.
--
--   Read it like a Header of the Meshes lateron in the Format
data IQMMesh = IQMMesh
                { meshName              :: Maybe Mesh
                , meshMaterial          :: Int32
                , meshFirstVertex       :: Int32
                , meshNumVertexes       :: Int32
                , meshFirstTriangle     :: Int32
                , meshNumTriangles      :: Int32
                } deriving (Show, Eq)

-- | Format of a whole IQM-File
--
--   still unfinished!
data IQM = IQM
        { header                :: IQMHeader
        , texts                 :: [ByteString]
        , meshes                :: [IQMMesh]
        } deriving (Show, Eq)

