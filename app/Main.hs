module Main where

-- This is basically like Haskell's default prelude minus a couple of footguns, plus a couple of useful things
import Blog.Prelude

-- This is imported unqualified because pretty much every function is from here
import Blog.Engine

-- engine related imports
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings

-- other libraries
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Development.Shake as Shake

make :: ReaderT Settings Shake.Rules ()
make = do
  -- point of entry: we want to generate the 'index.html' file
  -- the 'index.html' rule will 'need' the rest of the website
  want [RelativePath "index.html"]
  want [RelativePath "CNAME"]

  -- this is a function that is used as `itemsCache ["post//*.md"]`
  -- it will go through all posts at the path and then parse the metadata
  -- the results are cached per path, so it runs only once
  --
  -- fields required by all posts/pages/wikis:
  --   - id (is used as the file name/path)
  --   - title
  --   - publish (dd-mm-yyyy date format, dates in the future means don't publish)
  -- optional fields:
  --   - tags (list of strings)
  --   - changelog (list of strings)
  --   - renderChangelog (boolean)
  --
  itemsCache <- initItemsCache

  -- "PHONY"-style rule to clean everything
  "clean" ~> removeOutput

  -- rule for 'index.html'
  "index.html" %> \path -> do
    -- trigger the rules for these paths
    need' ["css//*", "images//*"]

    -- grab all the posts
    posts <- itemsCache ["post//*.md"]
    putVerbose $ "found " <> show (length posts) <> " post(s)"

    -- grab all the pages
    pages <- itemsCache ["page//*.md"]
    putVerbose $ "found " <> show (length posts) <> " page(s)"

    -- grab all the wiki pages
    wikis <- itemsCache ["wiki//*.md"]
    putVerbose $ "found " <> show (length posts) <> " wiki page(s)"

    -- trigger the rules for posts, pages, and wiki pages
    need . fmap (RelativePath . (-<.> "html") . ("post/" </>) . T.unpack . id . fst) $ posts
    need . fmap (RelativePath . (-<.> "html") . ("page/" </>) . T.unpack . id . fst) $ pages
    need . fmap (RelativePath . (-<.> "html") . ("wiki/" </>) . T.unpack . id . fst) $ wikis

    -- sort the posts by publish date
    let
      sortedPosts = fmap snd . sortOn (Down . publish . fst) $ posts
    -- write the file using the provided template, replacing it
    -- with the json data from the 'posts'
    writeFile (RelativePath "template/index.html") path
      . withMetadataObject "posts"
      . Aeson.toJSON
      $ sortedPosts

  -- static content, just copy files
  "css//*" %> \path -> copyFile path path
  "images//*" %> \path -> copyFile path path

  -- posts
  "post//*.html" %> \path -> do
    -- make sure we copy the content
    need' ["post/content//*"]

    -- get the post cache
    itemsCache ["post//*.md"]
      -- ... and generate the page using the provided template
      -- we can always write our own `generatePage` if we need to customize further
      --
      -- note: there's a bit of a trick here, `path` is something like `docs/post/foo.html`
      -- and we assume that the post `itemsCache` will contain an item with `id: foo` in its
      -- metadata; if not, this will error.
      --
      -- note: this function also uses 'need' for every tag found in the metadata of the post
      --       (does nothing if there are none)
      --
      -- arguments are:
      --   - "post" is the name used in error messages (e.g. "cannot find 'post' with id...")
      --     and also used as the object name passed to the mustache template
      --   - path is a destination `RelativePath "post/foo.html"`
      --   - last argument is a `RelativePath` to the source template
      >>= generatePage "post" path (RelativePath "template/post.html")
  "post/content//*" %> \path -> copyFile path path

  -- pages are identical to posts, see posts for details
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    itemsCache ["page//*.md"] >>= generatePage "page" path (RelativePath "template/page.html")
  "page/content//*" %> \path -> copyFile path path

  -- wiki pages are identical to posts, see posts for details
  -- we currently don't have any, so this does nothing
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    itemsCache ["wiki//*.md"] >>= generatePage "wiki" path (RelativePath "template/wiki.html")
  "wiki/content//*" %> \path -> copyFile path path

  -- tags, will generate `tag/name.html` for tags referenced by posts, pages, or wiki pages
  "tag/*.html" %> \path -> do
    -- grab all posts, pages, and wikis
    allPosts <- itemsCache ["post//*.md"]
    allPages <- itemsCache ["page//*.md"]
    allWikis <- itemsCache ["wiki//*.md"]

    let
      -- path is `RelativePath "tag/some-tag.html"`, tagName will be just `some-tag`
      tagName = T.pack . takeBaseName $ path
      -- find all posts, pages, wikis that contain `some-tag`
      posts = filter ((tagName `elem`) . tags . fst) allPosts
      pages = filter ((tagName `elem`) . tags . fst) allPages
      wikis = filter ((tagName `elem`) . tags . fst) allWikis

    -- generate the tag file and pass a list for each of posts, pages, and wikis
    writeFile (RelativePath "template/tag.html") path
      . addKey "posts" (Aeson.toJSON $ fmap snd posts)
      . addKey "pages" (Aeson.toJSON $ fmap snd pages)
      . addKey "wikis" (Aeson.toJSON $ fmap snd wikis)
      . withMetadataObject "tagName"
      . Aeson.toJSON
      $ tagName

  -- needed by github pages
  "CNAME" %> \path -> copyFile path path

-- this is the entry point for the program
main :: IO ()
main = do
  let
    -- define output and source as 'docs' and 'site'
    settings = Settings {Settings.output = "docs", Settings.source = "site"}
    -- use the settings below
    shakeOpts = mkShakeOpts settings
   in
    -- Shake is kind of like F#'s FAKE, but ya know, better :p
    -- basically a DSL for build systems, which website-engine is built on top of
    Shake.shakeArgs shakeOpts do
      -- this is basically just some monadic magic that allows passing `settings`
      -- automagically inside the `make` function without explicitly doing so :)
      -- `ReaderT` is essentially the "I have some context" monad
      -- and the context is `settings`
      runReaderT make settings
 where
  mkShakeOpts :: Settings -> Shake.ShakeOptions
  mkShakeOpts opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = [Settings.source opts]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }
