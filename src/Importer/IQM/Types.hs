module Importer.IQM.Types where

import Data.Int
import Data.ByteString
import Data.Attoparsec.ByteString.Char8
import Control.Monad.Trans.State.Lazy (StateT)

newtype Mesh = Mesh Int32 deriving (Show, Eq)
type CParser a = StateT Int64 Parser a

-- Int32 or Int64 - depending on implementation. Format just specifies "uint".
-- 4-Byte indicates Int32

-- | ofs_* fields are relative tot he beginning of the iqmheader struct
--   ofs_* fields are set to 0 when data is empty
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
 
 
data IQMMesh = IQMMesh
                { meshName              :: Maybe Mesh
                , meshMaterial          :: Int32
                , meshFirstVertex       :: Int32
                , meshNumVertexes       :: Int32
                , meshFirstTriangle     :: Int32
                , meshNumTriangles      :: Int32
                } deriving (Show, Eq)
                
data IQM = IQM
        { header                :: IQMHeader
        , texts                 :: [ByteString]
        , meshes                :: [IQMMesh]
        } deriving (Show, Eq)
        
