if (!self.define) {
  const e = (e) => {
      'require' !== e && (e += '.js')
      let s = Promise.resolve()
      return (
        n[e] ||
          (s = new Promise(async (s) => {
            if ('document' in self) {
              const n = document.createElement('script')
              ;(n.src = e), document.head.appendChild(n), (n.onload = s)
            } else importScripts(e), s()
          })),
        s.then(() => {
          if (!n[e]) throw new Error(`Module ${e} didn’t register its module`)
          return n[e]
        })
      )
    },
    s = (s, n) => {
      Promise.all(s.map(e)).then((e) => n(1 === e.length ? e[0] : e))
    },
    n = { require: Promise.resolve(s) }
  self.define = (s, i, t) => {
    n[s] ||
      (n[s] = Promise.resolve().then(() => {
        let n = {}
        const r = { uri: location.origin + s.slice(1) }
        return Promise.all(
          i.map((s) => {
            switch (s) {
              case 'exports':
                return n
              case 'module':
                return r
              default:
                return e(s)
            }
          }),
        ).then((e) => {
          const s = t(...e)
          return n.default || (n.default = s), n
        })
      }))
  }
}
define('./sw.js', ['./workbox-ea903bce'], function (e) {
  'use strict'
  importScripts(),
    self.skipWaiting(),
    e.clientsClaim(),
    e.precacheAndRoute(
      [
        {
          url: '/_next/static/chunks/4.2dee5d830195ddd06029.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/906-7b9696c9b17c64b94384.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/d25bd147-65fcc4c92edba8b370fb.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/fb7d5399-0d6001c72a29ebec41eb.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/framework-2191d16384373197bc0a.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/main-1b0f1fd287f08bad6012.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/pages/_app-6ba3a11e93bdf6a85175.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/pages/_error-a0e21b9b223f827fe1f2.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/pages/index-5cea1a6a4f484642ff08.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/polyfills-a54b4f32bdc1ef890ddd.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/chunks/webpack-a1ea085630ce50807058.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        { url: '/_next/static/css/331301db207a91d407e5.css', revision: 'iq9sQ07m6d_SiAb-0HX08' },
        {
          url: '/_next/static/iq9sQ07m6d_SiAb-0HX08/_buildManifest.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        {
          url: '/_next/static/iq9sQ07m6d_SiAb-0HX08/_ssgManifest.js',
          revision: 'iq9sQ07m6d_SiAb-0HX08',
        },
        { url: '/favicon.ico', revision: 'c30c7d42707a47a3f4591831641e50dc' },
        { url: '/manifest.json', revision: '6fefee72ca361eb4dfafb128818c8424' },
        { url: '/vercel.svg', revision: '4b4f1876502eb6721764637fe5c41702' },
      ],
      { ignoreURLParametersMatching: [] },
    ),
    e.cleanupOutdatedCaches(),
    e.registerRoute(
      '/',
      new e.NetworkFirst({
        cacheName: 'start-url',
        plugins: [
          {
            cacheWillUpdate: async ({ request: e, response: s, event: n, state: i }) =>
              s && 'opaqueredirect' === s.type
                ? new Response(s.body, { status: 200, statusText: 'OK', headers: s.headers })
                : s,
          },
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /^https:\/\/fonts\.(?:gstatic)\.com\/.*/i,
      new e.CacheFirst({
        cacheName: 'google-fonts-webfonts',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 4, maxAgeSeconds: 31536e3, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /^https:\/\/fonts\.(?:googleapis)\.com\/.*/i,
      new e.StaleWhileRevalidate({
        cacheName: 'google-fonts-stylesheets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 4, maxAgeSeconds: 604800, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:eot|otf|ttc|ttf|woff|woff2|font.css)$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'static-font-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 4, maxAgeSeconds: 604800, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:jpg|jpeg|gif|png|svg|ico|webp)$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'static-image-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 64, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\/_next\/image\?url=.+$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'next-image',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 64, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:mp3|mp4)$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'static-media-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:js)$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'static-js-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:css|less)$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'static-style-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\/_next\/data\/.+\/.+\.json$/i,
      new e.StaleWhileRevalidate({
        cacheName: 'next-data',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      /\.(?:json|xml|csv)$/i,
      new e.NetworkFirst({
        cacheName: 'static-data-assets',
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      ({ url: e }) => {
        if (!(self.origin === e.origin)) return !1
        const s = e.pathname
        return !s.startsWith('/api/auth/') && !!s.startsWith('/api/')
      },
      new e.NetworkFirst({
        cacheName: 'apis',
        networkTimeoutSeconds: 10,
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 16, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      ({ url: e }) => {
        if (!(self.origin === e.origin)) return !1
        return !e.pathname.startsWith('/api/')
      },
      new e.NetworkFirst({
        cacheName: 'others',
        networkTimeoutSeconds: 10,
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 86400, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    ),
    e.registerRoute(
      ({ url: e }) => !(self.origin === e.origin),
      new e.NetworkFirst({
        cacheName: 'cross-origin',
        networkTimeoutSeconds: 10,
        plugins: [
          new e.ExpirationPlugin({ maxEntries: 32, maxAgeSeconds: 3600, purgeOnQuotaError: !0 }),
        ],
      }),
      'GET',
    )
})
