module Main where

-- This is basically like Haskell's default prelude minus a couple of footguns, plus a couple of useful things
import Blog.Prelude

-- This is imported unqualified because pretty much every function is from here
import Blog.Engine

-- engine related imports
import Blog.Item
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

  -- CNAME is needed for github pages
  want [RelativePath "CNAME"]

  let
    post = ("post", ["post//*.md"])
    page = ("page", ["page//*.md"])
    wiki = ("wiki", ["wiki//*.md"])

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
  -- Item contain the following extra fields
  --   - metadata contains all the above fields, plus any custom fields
  --   - documentContent which is the pandoc parsed content
  itemsCache <- initItemsCache

  -- "PHONY"-style rule to clean everything
  "clean" ~> removeOutput

  -- rule for 'index.html'
  "index.html" %> \path -> do
    -- trigger the rules for these paths
    need' ["css//*", "images//*"]

    -- grab all the posts
    posts <- itemsCache post
    pages <- itemsCache page
    wikis <- itemsCache wiki

    -- trigger the rules for posts, pages, and wiki pages
    need . fmap (RelativePath . ("post" </>) . (-<.> "html") . T.unpack . id) . snd $ posts
    need . fmap (RelativePath . ("page" </>) . (-<.> "html") . T.unpack . id) . snd $ pages
    need . fmap (RelativePath . ("wiki" </>) . (-<.> "html") . T.unpack . id) . snd $ wikis

    -- sort the posts by publish date
    let
      sortedPosts = sortOn (Down . publish) . snd $ posts
    -- write the file using the provided template, replacing it
    -- with the json data from the 'posts'
    writeFile (RelativePath "template/index.html") path
      . withMetadataObject "posts"
      . Aeson.toJSON
      . fmap metadata
      $ sortedPosts

  -- static content, just copy files
  "css//*" %> \path -> copyFile path path
  "images//*" %> \path -> copyFile path path

  -- posts
  "post//*.html" %> \path -> do
    -- make sure we copy the content
    need' ["post/content//*"]

    -- get the cache for all posts, pages, and wikis;
    -- the cache will be a list, where each item is a tuple
    -- the tuple has a string key, i.e. "post", "page", "wiki"
    -- the tuple also has a list of all items under that key
    -- (essentially this is a dictionary with key and items)
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      -- ... and generate the page using the provided template
      -- we can always write our own `generatePage` if we need to customize further
      --
      -- note: there's a bit of a trick here, `path` is something like `docs/post/foo.html`
      -- and we assume that:
      --   - there exists some key in the dictionary "post" (same as the first argument),
      --   - under that key, there will be an item with `id: foo`.
      --
      -- note: this function also uses 'need' for every tag found in the metadata of the post
      --       (does nothing if there are none)
      --
      -- arguments are:
      --   - "post" is the name used in error messages (e.g. "cannot find 'post' with id..."),
      --     the object name passed to the mustache template,
      --     and as the key in the dictionary;
      --   - path is a destination `RelativePath "post/foo.html"`
      --   - last argument is a `RelativePath` to the source template
      >>= generatePage "post" path (RelativePath "template/post.html")
  "post/content//*" %> \path -> copyFile path path

  -- pages are identical to posts, see posts for details
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "page" path (RelativePath "template/page.html")
  "page/content//*" %> \path -> copyFile path path

  -- wiki pages are identical to posts, see posts for details
  -- we currently don't have any, so this does nothing
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "wiki" path (RelativePath "template/wiki.html")
  "wiki/content//*" %> \path -> copyFile path path

  -- tags, will generate `tag/name.html` for tags referenced by posts, pages, or wiki pages
  "tag/*.html" %> \path -> do
    -- grab all posts, pages, and wikis
    allPosts <- itemsCache post
    allPages <- itemsCache page
    allWikis <- itemsCache wiki

    let
      -- path is `RelativePath "tag/some-tag.html"`, tagName will be just `some-tag`
      tagName = T.pack . takeBaseName $ path
      -- find all posts, pages, wikis that contain `some-tag`
      posts = filter ((tagName `elem`) . tags) . snd $ allPosts
      pages = filter ((tagName `elem`) . tags) . snd $ allPages
      wikis = filter ((tagName `elem`) . tags) . snd $ allWikis

    -- generate the tag file and pass a list for each of posts, pages, and wikis
    writeFile (RelativePath "template/tag.html") path
      . addKey "posts" (Aeson.toJSON $ fmap metadata posts)
      . addKey "pages" (Aeson.toJSON $ fmap metadata pages)
      . addKey "wikis" (Aeson.toJSON $ fmap metadata wikis)
      . withMetadataObject "tagName"
      . Aeson.toJSON
      $ tagName

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
      , Shake.shakeVerbosity = Shake.Info
      , Shake.shakeProgress = Shake.progressSimple
      }
