{-# LANGUAGE RankNTypes #-}

-- | Parser for IQM-Files
--
--   Assumes that the file is stored with 32-Bit-BigEndian-Ints
module Importer.IQM.Parser (parseIQM) where

import Importer.IQM.Types
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import Data.ByteString.Char8 (pack)
import Data.ByteString (split, null, ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.ByteString as B
import Graphics.GLUtil
import Graphics.Rendering.OpenGL.GL.StateVar (($=))
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GL.VertexArrayObjects
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.ObjectName
import Data.Word
import Data.Int
import Unsafe.Coerce
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable (sizeOf)

import Prelude as P hiding (take, null)

import Render.Misc (printPtrAsFloatArray, printPtrAsUByteArray, printPtrAsWord32Array, withVBO, checkError)

-- | helper-function for creating an integral out of [8-Bit Ints]
_w8ToInt :: Integral a => a -> a -> a
_w8ToInt i add = 256*i + add

-- | shorthand-function for parsing Word8 into Integrals
_parseNum :: (Integral a, Integral b) => [a] -> b
_parseNum = foldl1 _w8ToInt . map fromIntegral

-- | read a 16-Bit Int from Parsing-Input and log 2 bytes in our Parsing-Monad
--
--   begins with _ to defeat ghc-warnings. Rename if used!
_int16 :: CParser Word16
_int16 = do
        ret <- lift $ do
                         a <- anyWord8 :: Parser Word8
                         b <- anyWord8 :: Parser Word8
                         return $ _parseNum [b,a]
        modify (+2)
        return ret

-- | read a 32-Bit Int from Parsing-Input and log 4 bytes in our Parsing-Monad
_int32 :: CParser Int32
_int32 = do
        ret <- lift $ do
                         a <- anyWord8 :: Parser Word8
                         b <- anyWord8 :: Parser Word8
                         c <- anyWord8 :: Parser Word8
                         d <- anyWord8 :: Parser Word8
                         return $ _parseNum [d,c,b,a]
        modify (+4)
        return ret

w32leCParser :: CParser Word32
w32leCParser = do
	ret <- lift anyWord32le
	modify (+4)
	return ret

-- | Parser for the header
readHeader :: CParser IQMHeader
readHeader = do
         _ <- lift $ string (pack "INTERQUAKEMODEL\0")
         modify (+16)
         v <- w32leCParser
         lift $ when (v /= 2) $ fail "Version /= 2.\nThis Parser only supports Version 2 of the InterQuake-Model IQM"
         -- when v /= 2 then fail parsing.
         size' <- w32leCParser
         flags' <- w32leCParser
         num_text' <- w32leCParser
         ofs_text' <- w32leCParser
         num_meshes' <- w32leCParser
         ofs_meshes' <- w32leCParser
         num_vertexarrays' <- w32leCParser
         num_vertexes' <- w32leCParser
         ofs_vertexarrays' <- w32leCParser
         num_triangles' <- w32leCParser
         ofs_triangles' <- w32leCParser
         ofs_adjacency' <- w32leCParser
         num_joints' <- w32leCParser
         ofs_joints' <- w32leCParser
         num_poses' <- w32leCParser
         ofs_poses' <- w32leCParser
         num_anims' <- w32leCParser
         ofs_anims' <- w32leCParser
         num_frames' <- w32leCParser
         num_framechannels' <- w32leCParser
         ofs_frames' <- w32leCParser
         ofs_bounds' <- w32leCParser
         num_comment' <- w32leCParser
         ofs_comment' <- w32leCParser
         num_extensions' <- w32leCParser
         ofs_extensions' <- w32leCParser
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
        name <- w32leCParser
        mat <- w32leCParser
        fv <- w32leCParser
        nv <- w32leCParser
        ft <- w32leCParser
        nt <- w32leCParser
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
        vat <- rawEnumToVAT =<< w32leCParser
        flags' <- w32leCParser
        format <- rawEnumToVAF =<< w32leCParser
        size <- w32leCParser
        offset <- w32leCParser
        return $ IQMVertexArray vat (fromIntegral flags') format (fromIntegral size) offset nullPtr

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
(.-) a b = fromIntegral a - fromIntegral b

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
--
--   Does a 2-Pass-Parsing. Reads in Structure on first pass (O(n))and
--   fills the Structure in a 2nd Pass from Offsets (O(memcpy'd bytes)).
parseIQM :: String -> IO IQM
parseIQM a =
    do
    f <- B.readFile a
    -- Parse Headers/Offsets to BareIQM
    let result = parse doIQMparse f
    bare <- case result of
        Done _ x -> return x
        y -> error $ show y
    -- Fill Vertex-Array with buffer objects and data of Offsets
    va' <- mapM (readInVAO f (num_vertexes.bareheader $ bare)) (barevertexArrays bare)

    -- create VAO with attached vbos
    vao <- makeVAO $ do
        -- generate array buffers
        --
        --for pos,normal,tex:
        let initBuffer :: AttribLocation -> IQMVertexArrayType -> [IQMVertexArray] -> Int -> IO ()
            initBuffer l t vas len' =
                do
                -- find array with type t, otherwise abort hard.
                let (IQMVertexArray _ _ format num _ dat) = case filter (\(IQMVertexArray ty _ _ _ _ _) -> ty == t) vas of
                                                    [b] -> b
                                                    _ -> error $ "Current object does not support " ++ (show t)
                buf <- genObjectName
                -- create buffer and write data
                withVBO buf (toBufferTargetfromVAType t) $ do
                    -- copy data
                    bufferData (toBufferTargetfromVAType t) $= (fromIntegral len' * fromIntegral num * (fromIntegral.vaSize) format,dat,StaticDraw)
                    checkError "bufferData vao"
                    -- tell layout
                    vertexAttribPointer l $= (ToFloat, VertexArrayDescriptor num Float 0 nullPtr)
        let len = (fromIntegral.num_triangles.bareheader) bare
        initBuffer (AttribLocation 0) IQMPosition va' len
        initBuffer (AttribLocation 1) IQMNormal   va' len
        initBuffer (AttribLocation 2) IQMTexCoord va' len

    -- for indices
    tbo <- genObjectName
    tris <- withVBO tbo ArrayBuffer $ do
        let
            len = (fromIntegral.num_triangles.bareheader) bare
            byteLen = len * 3 * sizeOf (undefined :: Word32)
            data' = skipDrop ((fromIntegral.ofs_triangles.bareheader) bare) byteLen f
        p <- mallocBytes byteLen
        unsafeUseAsCString data' (\s -> copyBytes p s byteLen)
        withVBO tbo ElementArrayBuffer $ do
            bufferData ElementArrayBuffer $= (fromIntegral byteLen, p, StaticDraw)
        checkError "bufferData tris"
        return $ castPtr p
    putStrLn "Triangles:"
    printPtrAsWord32Array tris ((*3).fromIntegral.num_triangles.bareheader $ bare) 3
    print bare
    return $ IQM
        { header               = bareheader bare
        , texts                = baretexts bare
        , meshes               = baremeshes bare
        , vertexArrays         = va'
        , vertexBufferObjects  = []
        , vertexArrayObject    = vao
        , triangles            = tris
        , triangleBufferObject = tbo
        }

createVAO :: [(IQMVertexArray, BufferObject)] -> IO ()
createVAO bo = do
        print bo
        initVAO (AttribLocation 0) IQMPosition bo
        initVAO (AttribLocation 1) IQMNormal   bo
        initVAO (AttribLocation 2) IQMTexCoord bo

initVAO :: AttribLocation -> IQMVertexArrayType -> [(IQMVertexArray, BufferObject)] -> IO ()
initVAO l t bo = do
	print $ concat ["adding ", show t, " to vertexBufferObject"]
	let (IQMVertexArray _ _ _ num _ _,buf) = case filter (\(IQMVertexArray ty _ _ _ _ _, _) -> ty == t) bo of
							[(a,b)] -> (a,b)
							_ -> error "IQM-Object not render-able with current shader-mechanics"
	bindBuffer (toBufferTargetfromVAType t) $= Just buf
	vertexAttribArray l $= Enabled
	vertexAttribPointer l $= (ToFloat, VertexArrayDescriptor num Float 0 nullPtr)

-- | Creates a BufferObject on the Graphicscard for each BufferObject

toVBOfromVAO :: IQMVertexArray -> IO BufferObject
toVBOfromVAO (IQMVertexArray type' _ _ num _ ptr) =
	fromPtr (toBufferTargetfromVAType type') (fromIntegral num) ptr

-- | translates from VA-type to BufferTarget

toBufferTargetfromVAType :: IQMVertexArrayType -> BufferTarget
toBufferTargetfromVAType IQMPosition      = ArrayBuffer
toBufferTargetfromVAType IQMTexCoord      = ArrayBuffer
toBufferTargetfromVAType IQMNormal        = ArrayBuffer
toBufferTargetfromVAType IQMBlendIndexes  = ElementArrayBuffer
toBufferTargetfromVAType IQMBlendWeights  = ArrayBuffer
toBufferTargetfromVAType IQMColor         = ArrayBuffer
toBufferTargetfromVAType _                = ArrayBuffer

-- | Allocates memory for the Vertex-data and copies it over there
--   from the given input-String
--
--   Note: The String-Operations are O(1), so only O(numberOfCopiedBytes)
--   is needed in term of computation.
readInVAO :: ByteString -> Word32 -> IQMVertexArray -> IO IQMVertexArray
readInVAO d vcount (IQMVertexArray type' a format num offset ptr) =
        do
        let
            numElems = fromIntegral vcount * fromIntegral num
            byteLen = numElems * vaSize format
            data' = skipDrop (fromIntegral offset) byteLen d

        unless (ptr == nullPtr) $ error $ "Error reading Vertex-Array: Double Read of " ++ show type'
        p <- mallocBytes byteLen
        putStrLn $ concat ["Allocating ", show vcount ,"x", show num,"x",show (vaSize format)," = ", show byteLen, " Bytes at ", show p, " for ", show type']
        putStrLn $ concat ["Filling starting at ", show offset, " with: "]
        unsafeUseAsCString data' (\s -> copyBytes p s byteLen)
        case type' of
            IQMBlendIndexes -> printPtrAsUByteArray p numElems 4
            IQMBlendWeights -> printPtrAsUByteArray p numElems 4
            IQMTexCoord     -> printPtrAsFloatArray p numElems 2
            _ -> printPtrAsFloatArray p numElems 3
        return $ IQMVertexArray type' a format num offset $ castPtr p

-- | Real internal Parser.
--
--   Consumes the String only once, thus in O(n). But all Data-Structures are
--   not allocated and copied. readInVAO has to be called on each one.
doIQMparse :: Parser BareIQM
doIQMparse =
    flip evalStateT 0 $ --evaluate parser with state starting at 0
        do
            h <- readHeader                                         --read header
            skipToCounter $ ofs_text h                              --skip 0-n bytes to get to text
            text <- lift . take . fromIntegral $ num_text h         --read texts
            modify . (+) . fromIntegral $ num_text h                --put offset forward
            skipToCounter $ ofs_meshes h                            --skip 0-n bytes to get to meshes
            meshes' <- readMeshes $ fromIntegral $ num_meshes h     --read meshes
            skipToCounter $ ofs_vertexarrays h            --skip 0-n bytes to get to Vertex-Arrays
            vaf <- readVAFs $ fromIntegral $ num_vertexarrays h     --read Vertex-Arrays
            return BareIQM
                    { bareheader = h
                    , baretexts = filter (not.null) (split (unsafeCoerce '\0') text)
                    , baremeshes = meshes'
                    , barevertexArrays = vaf
                    }

-- | Helper-Function for Extracting a random substring out of a Bytestring
--   by the Offsets provided.
--
--   O(1).
skipDrop :: Int -- ^ Bytes to drop
         -> Int -- ^ Bytes to take
         -> ByteString
         -> ByteString
skipDrop a b= B.take b . B.drop a
