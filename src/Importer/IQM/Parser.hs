{-# LANGUAGE RankNTypes #-}

-- | Parser for IQM-Files
--
--   Assumes that the file is stored with 32-Bit-BigEndian-Ints
module Importer.IQM.Parser (parseIQM) where

import Importer.IQM.Types
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString
import Data.ByteString.Char8 (pack)
import Data.ByteString (split, null)
import Data.Word
import Data.Int
import Unsafe.Coerce
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad

import Prelude as P hiding (take, null)

-- | helper-function for creating an integral out of [8-Bit Ints]
w8ToInt :: Integral a => a -> a -> a
w8ToInt i add = 256*i + add

-- | shorthand-function for parsing Word8 into Integrals
parseNum :: (Integral a, Integral b) => [a] -> b
parseNum = (foldl1 w8ToInt) . map fromIntegral

-- | read a 16-Bit Int from Parsing-Input and log 2 bytes in our Parsing-Monad
--
--   begins with _ to defeat ghc-warnings. Rename if used!
_int16 :: CParser Word16
_int16 = do
        ret <- lift $ do
                         a <- anyWord8 :: Parser Word8
                         b <- anyWord8 :: Parser Word8
                         return $ parseNum [b,a]
        modify (+2)
        return ret

-- | read a 32-Bit Int from Parsing-Input and log 4 bytes in our Parsing-Monad
int32 :: CParser Word32
int32 = do
        ret <- lift $ do
                         a <- anyWord8 :: Parser Word8
                         b <- anyWord8 :: Parser Word8
                         c <- anyWord8 :: Parser Word8
                         d <- anyWord8 :: Parser Word8
                         return $ parseNum [d,c,b,a]
        modify (+4)
        return $ ret

-- | Parser for the header
readHeader :: CParser IQMHeader
readHeader = do
         _ <- lift $ string (pack "INTERQUAKEMODEL\0")
         modify (+16)
         v <- int32
         -- when v /= 2 then --TODO: error something
         size' <- int32
         flags' <- int32
         num_text' <- int32
         ofs_text' <- int32
         num_meshes' <- int32
         ofs_meshes' <- int32
         num_vertexarrays' <- int32
         num_vertexes' <- int32
         ofs_vertexarrays' <- int32
         num_triangles' <- int32
         ofs_triangles' <- int32
         ofs_adjacency' <- int32
         num_joints' <- int32
         ofs_joints' <- int32
         num_poses' <- int32
         ofs_poses' <- int32
         num_anims' <- int32
         ofs_anims' <- int32
         num_frames' <- int32
         num_framechannels' <- int32
         ofs_frames' <- int32
         ofs_bounds' <- int32
         num_comment' <- int32
         ofs_comment' <- int32
         num_extensions' <- int32
         ofs_extensions' <- int32
         return IQMHeader { version = v
                , filesize           = size'
                , flags              = fromIntegral flags'
                , num_text           = num_text'
                , ofs_text           = ofs_text'
                , num_meshes         = num_meshes'
                , ofs_meshes         = ofs_meshes'
                , num_vertexarrays   = num_vertexarrays'
                , num_vertexes       = num_vertexes'
                , ofs_vertexarrays   = ofs_vertexarrays'
                , num_triangles      = num_triangles'
                , ofs_triangles      = ofs_triangles'
                , ofs_adjacency      = ofs_adjacency'
                , num_joints         = num_joints'
                , ofs_joints         = ofs_joints'
                , num_poses          = num_poses'
                , ofs_poses          = ofs_poses'
                , num_anims          = num_anims'
                , ofs_anims          = ofs_anims'
                , num_frames         = num_frames'
                , num_framechannels  = num_framechannels'
                , ofs_frames         = ofs_frames'
                , ofs_bounds         = ofs_bounds'
                , num_comment        = num_comment'
                , ofs_comment        = ofs_comment'
                , num_extensions     = num_extensions'
                , ofs_extensions     = ofs_extensions'
                }

-- | Parser for Mesh-Structure
readMesh :: CParser IQMMesh
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

-- | helper to read n consecutive Meshes tail-recursive
readMeshes :: Int -> CParser [IQMMesh]
readMeshes 1 = do
        m <- readMesh
        return [m]
readMeshes n = do
        m <- readMesh
        ms <- readMeshes (n-1)
        return $ m:ms

-- | Parser for Mesh-Structure
readVAF :: CParser IQMVertexArray
readVAF = do
        vat <- rawEnumToVAT =<< int32
        flags' <- int32
        format <- rawEnumToVAF =<< int32
        size <- int32
        offset <- int32
        return $ IQMVertexArray vat (fromIntegral flags') format (fromIntegral size) offset

-- | helper to read n consecutive Meshes tail-recursive
readVAFs :: Int -> CParser [IQMVertexArray]
readVAFs 1 = do
        f <- readVAF
        return [f]
readVAFs n = do
        f <- readVAF
        fs <- readVAFs (n-1)
        return $ f:fs

-- | helper-Notation for subtracting 2 integral values of different kind in the precision
--   of the target-kind
(.-) :: forall a a1 a2.
              (Num a, Integral a2, Integral a1) =>
              a1 -> a2 -> a
(.-) a b = (fromIntegral a) - (fromIntegral b)

infix 5 .-

-- | skips (=drops) all input until the internal counter is at a given bytecount
--
--   Fails the parser if given bytecount is lower than the internal counter as we
--   read sequentially and do not do backtracking
skipToCounter :: Integral a => a -> CParser ()
skipToCounter a = do
                        let d = fromIntegral a
                        c <- get
                        when (d < c) $ fail "wanting to skip to counter already passed"
                        _ <- lift $ take $ d .- c
                        put d

-- | Parses an IQM-File and handles back the Haskell-Structure
parseIQM :: CParser IQM
parseIQM = do
        put 0                                                   --start at offset 0
        h <- readHeader                                         --read header
        skipToCounter $ ofs_text h                              --skip 0-n bytes to get to text
        text <- lift . take . fromIntegral $ num_text h         --read texts
        modify . (+) . fromIntegral $ num_text h                --put offset forward
        skipToCounter $ ofs_meshes h                            --skip 0-n bytes to get to meshes
        meshes' <- readMeshes (fromIntegral (num_meshes h))     --read meshes
        skipToCounter $ ofs_vertexarrays h                      --skip 0-n byots to get to vertexarray definition
        va <- readVAFs (fromIntegral (num_vertexarrays h))      --read them
        return IQM
                { header = h
                , texts = filter (not.null) (split (unsafeCoerce '\0') text)
                , meshes = meshes'
                , vertexArrays = va
                }

