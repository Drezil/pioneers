module Importer.IQM.Parser where

import Importer.IQM.Types
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString
import Data.Attoparsec
import Data.ByteString.Char8 (pack)
import Data.ByteString (split, null)
import Data.Word
import Data.Int
import Data.List (foldl1)
import Foreign.C.Types
import Unsafe.Coerce

import Prelude as P hiding (take, null)

w8Toint :: Integral a => a -> a -> a
w8Toint i add = 256*i + add


int16 :: Parser Int16
int16 = do
        a <- anyWord8 :: Parser Word8
        b <- anyWord8 :: Parser Word8
        return $ foldl1 w8Toint $ map fromIntegral [b,a]

int32 :: Parser Int32
int32 = do
        a <- anyWord8 :: Parser Word8
        b <- anyWord8 :: Parser Word8
        c <- anyWord8 :: Parser Word8
        d <- anyWord8 :: Parser Word8
        return $ foldl1 w8Toint $ map fromIntegral [d,c,b,a]

readHeader = do
         string (pack "INTERQUAKEMODEL\0")
         v <- int32
         -- when v /= 2 then --TODO: error something 
         size <- int32
         flags <- int32
         num_text <- int32
         ofs_text <- int32         
         num_meshes <- int32
         ofs_meshes <- int32         
         num_vertexarrays <- int32
         num_vertexes <- int32         
         ofs_vertexarrays <- int32
         num_triangles <- int32         
         ofs_triangles <- int32
         ofs_adjacency <- int32         
         num_joints <- int32
         ofs_joints <- int32         
         num_poses <- int32
         ofs_poses <- int32         
         num_anims <- int32
         ofs_anims <- int32         
         num_frames <- int32
         num_framechannels <- int32         
         ofs_frames <- int32
         ofs_bounds <- int32         
         num_comment <- int32
         ofs_comment <- int32         
         num_extensions <- int32
         ofs_extensions <- int32
         return (IQMHeader { version = v
                , filesize           = size
                , flags              = flags
                , num_text           = num_text
                , ofs_text           = ofs_text
                , num_meshes         = num_meshes
                , ofs_meshes         = ofs_meshes
                , num_vertexarrays   = num_vertexarrays
                , num_vertexes       = num_vertexes
                , ofs_vertexarrays   = ofs_vertexarrays
                , num_triangles      = num_triangles
                , ofs_triangles      = ofs_triangles
                , ofs_adjacency      = ofs_adjacency
                , num_joints         = num_joints
                , ofs_joints         = ofs_joints
                , num_poses          = num_poses
                , ofs_poses          = ofs_poses
                , num_anims          = num_anims
                , ofs_anims          = ofs_anims
                , num_frames         = num_frames
                , num_framechannels  = num_framechannels
                , ofs_frames         = ofs_frames
                , ofs_bounds         = ofs_bounds
                , num_comment        = num_comment
                , ofs_comment        = ofs_comment
                , num_extensions     = num_extensions
                , ofs_extensions     = ofs_extensions
                }
                ,  16+27*4)

readMesh :: Parser IQMMesh
readMesh = do
        name <- int32
        mat <- int32
        fv <- int32
        nv <- int32
        ft <- int32
        nt <- int32
        return IQMMesh
                { meshName              = if name == 0 then Nothing else Just (Mesh name)
                , meshMaterial          = mat
                , meshFirstVertex       = fv
                , meshNumVertexes       = nv
                , meshFirstTriangle     = ft
                , meshNumTriangles      = nt
                }

readMeshes :: Int -> Parser [IQMMesh]
readMeshes 1 = do
        m <- readMesh
        return [m]
readMeshes n = do
        m <- readMesh
        ms <- readMeshes (n-1)
        return $ m:ms
                
parseIQM :: Parser IQM
parseIQM = do
        (h,soFar) <- readHeader
        take $ (fromIntegral (ofs_text h)) - soFar
        text <- take $ fromIntegral $ num_text h
        meshes <- readMeshes (fromIntegral (num_meshes h))
        return IQM
                { header = h
                , texts = filter (not.null) (split (unsafeCoerce '\0') text)
                , meshes = meshes
                }
                