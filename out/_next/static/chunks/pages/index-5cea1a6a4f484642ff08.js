;(self.webpackChunk_N_E = self.webpackChunk_N_E || []).push([
  [405],
  {
    374: function (e, n, t) {
      'use strict'
      t.r(n),
        t.d(n, {
          Graph: function () {
            return je
          },
          GraphPage: function () {
            return fe
          },
          default: function () {
            return xe
          },
        })
      var i = t(7757),
        r = t.n(i),
        o = t(2137),
        l = t(5893),
        s = t(7329),
        c = t(6156),
        a = t(4699),
        d = t(7294)
      function u(e, n) {
        var t,
          i = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {},
          r = h(e, null !== (t = i.storage) && void 0 !== t ? t : localStorage),
          o = r.get(),
          l = void 0 !== o ? o : n
        l !== o && r.update(l)
        var s = (0, d.useState)(l),
          c = s[0],
          a = s[1]
        ;(0, d.useEffect)(
          function () {
            c !== l && a(l)
          },
          [e],
        )
        var u = function (e) {
          e instanceof Function
            ? a(function (n) {
                var t = e(n)
                return r.update(t), t
              })
            : (a(e), r.update(e))
        }
        return [c, u]
      }
      function h(e, n) {
        return {
          get: function () {
            var t = n.getItem(e)
            if (t && 'undefined' !== t) return JSON.parse(t)
          },
          update: function (t) {
            n.setItem(e, JSON.stringify(t))
          },
          remove: function () {
            n.removeItem(e)
          },
        }
      }
      var g = t(4533),
        x = t(4309),
        f = t(2351),
        j = t(980),
        p = t(8017),
        m = t(6194),
        b = [],
        v = {}
      for (var y in m.oY)
        for (var C in m.oY[y]) {
          var k = y + C
          'LinearNone' === k && (k = 'Linear'), b.push(k), (v[k] = m.oY[y][C])
        }
      var w = v,
        S = {
          enabled: !0,
          charge: -700,
          collision: !0,
          collisionStrength: 20,
          centering: !0,
          centeringStrength: 0.05,
          linkStrength: 0.1,
          linkIts: 1,
          alphaDecay: 0.02,
          alphaTarget: 0,
          alphaMin: 0,
          velocityDecay: 0.25,
          gravity: 0.3,
          gravityOn: !0,
        },
        I = { orphans: !1, parents: !0, tags: [], nodes: [], links: [], date: [] },
        O = {
          particles: !1,
          particlesNumber: 0,
          particlesWidth: 4,
          linkOpacity: 0.8,
          linkWidth: 1,
          nodeRel: 4,
          nodeOpacity: 1,
          nodeResolution: 12,
          labels: 2,
          labelScale: 1.5,
          highlight: !0,
          highlightNodeSize: 2,
          highlightLinkSize: 2,
          highlightAnim: !0,
          animationSpeed: 700,
          algorithmOptions: b,
          algorithmName: 'BackOut',
          linkColorScheme: 'gray.500',
          nodeColorScheme: [
            'red.500',
            'gray.600',
            'yellow.500',
            'green.500',
            'cyan.500',
            'blue.500',
            'pink.500',
            'purple.500',
            'orange.500',
          ],
          nodeHighlight: '',
          linkHighlight: 'purple.500',
          backgroundColor: 'white',
          emacsNodeColor: 'gray.800',
          labelTextColor: 'black',
          labelBackgroundColor: 'white',
          labelBackgroundOpacity: 0.7,
        },
        N = { follow: 'zoom', localSame: 'add', zoomPadding: 200, zoomSpeed: 2e3 },
        z = { highlight: 'hover', local: 'click', follow: 'double' },
        R = t(7375),
        T = t(3924),
        D = t(3986),
        H = t(9641),
        E = t(7546),
        Z = t(3441),
        P = t(6569),
        F = t(4189),
        L = t(454),
        B = t(8420),
        M = t(6699),
        W = t(155),
        X = t(6769),
        A = t(336),
        _ = t(2026),
        q = t(4096),
        U = t(4115),
        Q = t(8134),
        V = t(8235),
        K = t(7273),
        J = t(5267),
        Y = t(6049),
        G = t(3014),
        $ = t(6658),
        ee = t(9356)
      function ne(e, n) {
        var t = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var i = Object.getOwnPropertySymbols(e)
          n &&
            (i = i.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            t.push.apply(t, i)
        }
        return t
      }
      function te(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? ne(Object(t), !0).forEach(function (n) {
                ;(0, c.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : ne(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var ie = function (e) {
          var n = e.physics,
            t = e.setPhysics,
            i = e.threeDim,
            r = e.setThreeDim,
            o = e.filter,
            c = e.setFilter,
            u = e.visuals,
            h = e.setVisuals,
            g = e.mouse,
            x = e.setMouse,
            f = e.behavior,
            j = e.setBehavior,
            m = (0, d.useState)(!0),
            b = m[0],
            v = m[1],
            y = (0, d.useContext)(ee.N),
            C = y.highlightColor,
            k = y.setHighlightColor,
            w = [
              'red.500',
              'orange.500',
              'yellow.500',
              'green.500',
              'cyan.500',
              'blue.500',
              'pink.500',
              'purple.500',
              'gray.400',
              'gray.500',
              'gray.600',
              'white',
              'black',
            ],
            F = [
              'black',
              'gray.100',
              'gray.200',
              'gray.300',
              'gray.400',
              'gray.500',
              'gray.600',
              'gray.700',
              'gray.800',
              'gray.900',
              'white',
            ]
          return (0, l.jsxs)(l.Fragment, {
            children: [
              (0, l.jsx)(L.R, {
                in: !b,
                children: (0, l.jsx)(p.xu, {
                  position: 'absolute',
                  zIndex: 'overlay',
                  marginTop: 10,
                  marginLeft: 10,
                  display: b ? 'none' : 'block',
                  children: (0, l.jsx)(B.h, {
                    'aria-label': 'Settings',
                    icon: (0, l.jsx)(T.e, {}),
                    onClick: function () {
                      return v(!0)
                    },
                  }),
                }),
              }),
              (0, l.jsx)(L.R, {
                in: b,
                children: (0, l.jsxs)(p.xu, {
                  bg: 'alt.100',
                  w: 'xs',
                  marginTop: 10,
                  marginLeft: 10,
                  borderRadius: 'xl',
                  maxH: 650,
                  paddingBottom: 5,
                  zIndex: 300,
                  position: 'relative',
                  boxShadow: 'xl',
                  children: [
                    (0, l.jsxs)(p.xu, {
                      display: 'flex',
                      justifyContent: 'space-between',
                      alignItems: 'center',
                      paddingRight: 2,
                      paddingTop: 1,
                      children: [
                        (0, l.jsx)(M.u, {
                          label: '2D',
                          children: (0, l.jsx)(W.z, {
                            onClick: function () {
                              return r(!i)
                            },
                            variant: 'ghost',
                            zIndex: 'overlay',
                            children: i ? '3D' : '2D',
                          }),
                        }),
                        (0, l.jsxs)(p.xu, {
                          display: 'flex',
                          alignItems: 'center',
                          children: [
                            (0, l.jsx)(M.u, {
                              label: 'Reset settings to defaults',
                              children: (0, l.jsx)(B.h, {
                                'aria-label': 'Reset Defaults',
                                icon: (0, l.jsx)(D.A, {}),
                                onClick: function () {
                                  h(O), c(I), x(z), t(S), j(N)
                                },
                                variant: 'none',
                                size: 'sm',
                              }),
                            }),
                            (0, l.jsx)(B.h, {
                              size: 'sm',
                              icon: (0, l.jsx)(H.T, {}),
                              'aria-label': 'Close Tweak Panel',
                              variant: 'ghost',
                              onClick: function () {
                                return v(!1)
                              },
                            }),
                          ],
                        }),
                      ],
                    }),
                    (0, l.jsx)($.ZP, {
                      autoHeight: !0,
                      autoHeightMax: 600,
                      autoHide: !0,
                      renderThumbVertical: function (e) {
                        var n = e.style,
                          t = (0, R.Z)(e, ['style'])
                        return (0, l.jsx)(
                          p.xu,
                          te(
                            te({}, t),
                            {},
                            { style: te(te({}, n), {}, { borderRadius: 10 }), bg: C },
                          ),
                        )
                      },
                      children: (0, l.jsxs)(X.UQ, {
                        allowMultiple: !0,
                        allowToggle: !0,
                        color: 'black',
                        children: [
                          (0, l.jsxs)(X.Qd, {
                            children: [
                              (0, l.jsxs)(X.KF, {
                                children: [
                                  (0, l.jsx)(X.XE, { marginRight: 2 }),
                                  (0, l.jsx)(A.X, { size: 'sm', children: 'Filter' }),
                                ],
                              }),
                              (0, l.jsx)(X.Hk, {
                                children: (0, l.jsxs)(_.gC, {
                                  spacing: 2,
                                  justifyContent: 'flex-start',
                                  divider: (0, l.jsx)(_.cX, { borderColor: 'gray.500' }),
                                  align: 'stretch',
                                  paddingLeft: 7,
                                  color: 'gray.800',
                                  children: [
                                    (0, l.jsxs)(q.k, {
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(U.x, { children: 'Orphans' }),
                                        (0, l.jsx)(Q.r, {
                                          onChange: function () {
                                            c(te(te({}, o), {}, { orphans: !o.orphans }))
                                          },
                                          isChecked: o.orphans,
                                        }),
                                      ],
                                    }),
                                    (0, l.jsxs)(q.k, {
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(U.x, {
                                          children: 'Link nodes with parent file',
                                        }),
                                        (0, l.jsx)(Q.r, {
                                          onChange: function () {
                                            c(te(te({}, o), {}, { parents: !o.parents }))
                                          },
                                          isChecked: o.parents,
                                        }),
                                      ],
                                    }),
                                  ],
                                }),
                              }),
                            ],
                          }),
                          (0, l.jsxs)(X.Qd, {
                            children: [
                              (0, l.jsx)(X.KF, {
                                display: 'flex',
                                justifyContent: 'space-between',
                                children: (0, l.jsxs)(p.xu, {
                                  display: 'flex',
                                  children: [
                                    (0, l.jsx)(X.XE, { marginRight: 2 }),
                                    (0, l.jsx)(A.X, { size: 'sm', children: 'Physics' }),
                                  ],
                                }),
                              }),
                              (0, l.jsxs)(X.Hk, {
                                children: [
                                  (0, l.jsxs)(_.gC, {
                                    spacing: 2,
                                    justifyContent: 'flex-start',
                                    divider: (0, l.jsx)(_.cX, { borderColor: 'gray.500' }),
                                    align: 'stretch',
                                    paddingLeft: 7,
                                    color: 'gray.800',
                                    children: [
                                      (0, l.jsx)(le, {
                                        label: 'Gravity',
                                        value: n.gravityOn,
                                        onChange: function () {
                                          return t(te(te({}, n), {}, { gravityOn: !n.gravityOn }))
                                        },
                                        children: (0, l.jsx)(oe, {
                                          label: 'Strength',
                                          value: 10 * n.gravity,
                                          onChange: function (e) {
                                            return t(te(te({}, n), {}, { gravity: e / 10 }))
                                          },
                                        }),
                                      }),
                                      (0, l.jsx)(oe, {
                                        value: -n.charge / 100,
                                        onChange: function (e) {
                                          return t(te(te({}, n), {}, { charge: -100 * e }))
                                        },
                                        label: 'Repulsive Force',
                                      }),
                                      (0, l.jsx)(le, {
                                        label: 'Collision',
                                        infoText: 'Perfomance sap, disable if slow',
                                        value: n.collision,
                                        onChange: function () {
                                          return t(te(te({}, n), {}, { collision: !n.collision }))
                                        },
                                        children: (0, l.jsx)(oe, {
                                          value: n.collisionStrength / 5,
                                          onChange: function (e) {
                                            return t(
                                              te(te({}, n), {}, { collisionStrength: 5 * e }),
                                            )
                                          },
                                          label: 'Collision Radius',
                                          infoText:
                                            'Easy with this one, high values can lead to a real jiggly mess',
                                        }),
                                      }),
                                      (0, l.jsx)(oe, {
                                        value: 5 * n.linkStrength,
                                        onChange: function (e) {
                                          return t(te(te({}, n), {}, { linkStrength: e / 5 }))
                                        },
                                        label: 'Link Force',
                                      }),
                                      (0, l.jsx)(oe, {
                                        label: 'Link Iterations',
                                        value: n.linkIts,
                                        onChange: function (e) {
                                          return t(te(te({}, n), {}, { linkIts: e }))
                                        },
                                        min: 0,
                                        max: 6,
                                        step: 1,
                                        infoText:
                                          'How many links down the line the physics of a single node affects (Slow)',
                                      }),
                                      (0, l.jsx)(oe, {
                                        label: 'Viscosity',
                                        value: 10 * n.velocityDecay,
                                        onChange: function (e) {
                                          return t(te(te({}, n), {}, { velocityDecay: e / 10 }))
                                        },
                                      }),
                                    ],
                                  }),
                                  (0, l.jsx)(p.xu, {
                                    children: (0, l.jsx)(X.UQ, {
                                      paddingLeft: 3,
                                      allowToggle: !0,
                                      children: (0, l.jsxs)(X.Qd, {
                                        children: [
                                          (0, l.jsxs)(X.KF, {
                                            children: [
                                              (0, l.jsx)(U.x, { children: 'Advanced' }),
                                              (0, l.jsx)(X.XE, { marginRight: 2 }),
                                            ],
                                          }),
                                          (0, l.jsx)(X.Hk, {
                                            children: (0, l.jsxs)(_.gC, {
                                              spacing: 2,
                                              justifyContent: 'flex-start',
                                              divider: (0, l.jsx)(_.cX, {
                                                borderColor: 'gray.500',
                                              }),
                                              align: 'stretch',
                                              paddingLeft: 3,
                                              color: 'gray.800',
                                              children: [
                                                (0, l.jsx)(oe, {
                                                  label: 'Stabilization rate',
                                                  value: 50 * n.alphaDecay,
                                                  onChange: function (e) {
                                                    return t(
                                                      te(te({}, n), {}, { alphaDecay: e / 50 }),
                                                    )
                                                  },
                                                }),
                                                (0, l.jsx)(le, {
                                                  label: 'Center nodes',
                                                  value: n.centering,
                                                  onChange: function () {
                                                    return t(
                                                      te(
                                                        te({}, n),
                                                        {},
                                                        { centering: !n.centering },
                                                      ),
                                                    )
                                                  },
                                                  infoText:
                                                    'Keeps the nodes in the center of the viewport. If disabled you can drag the nodes anywhere you want.',
                                                  children: (0, l.jsx)(oe, {
                                                    label: 'Centering Strength',
                                                    value: n.centeringStrength,
                                                    max: 2,
                                                    step: 0.01,
                                                    onChange: function (e) {
                                                      return t(
                                                        te(te({}, n), {}, { centeringStrength: e }),
                                                      )
                                                    },
                                                  }),
                                                }),
                                              ],
                                            }),
                                          }),
                                        ],
                                      }),
                                    }),
                                  }),
                                ],
                              }),
                            ],
                          }),
                          (0, l.jsxs)(X.Qd, {
                            children: [
                              (0, l.jsxs)(X.KF, {
                                children: [
                                  (0, l.jsx)(X.XE, { marginRight: 2 }),
                                  (0, l.jsx)(A.X, { size: 'sm', children: 'Visual' }),
                                ],
                              }),
                              (0, l.jsx)(X.Hk, {
                                children: (0, l.jsxs)(_.gC, {
                                  justifyContent: 'flex-start',
                                  align: 'stretch',
                                  children: [
                                    (0, l.jsx)(X.UQ, {
                                      allowToggle: !0,
                                      defaultIndex: [0],
                                      paddingLeft: 3,
                                      children: (0, l.jsxs)(X.Qd, {
                                        children: [
                                          (0, l.jsx)(X.KF, {
                                            children: (0, l.jsxs)(q.k, {
                                              justifyContent: 'space-between',
                                              w: '100%',
                                              children: [
                                                (0, l.jsx)(U.x, { children: 'Colors' }),
                                                (0, l.jsx)(X.XE, { marginRight: 2 }),
                                              ],
                                            }),
                                          }),
                                          (0, l.jsx)(X.Hk, {
                                            children: (0, l.jsx)(_.gC, {
                                              spacing: 2,
                                              justifyContent: 'flex-start',
                                              divider: (0, l.jsx)(_.cX, {
                                                borderColor: 'gray.500',
                                              }),
                                              align: 'stretch',
                                              color: 'gray.800',
                                              children: (0, l.jsxs)(p.xu, {
                                                children: [
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Nodes' }),
                                                      (0, l.jsx)(M.u, {
                                                        label: 'Shuffle node colors',
                                                        children: (0, l.jsx)(B.h, {
                                                          'aria-label': 'Shuffle node colors',
                                                          size: 'sm',
                                                          icon: (0, l.jsx)(E.n, {}),
                                                          variant: 'ghost',
                                                          onClick: function () {
                                                            var e,
                                                              n =
                                                                null !== (e = u.nodeColorScheme) &&
                                                                void 0 !== e
                                                                  ? e
                                                                  : []
                                                            h(
                                                              te(
                                                                te({}, u),
                                                                {},
                                                                {
                                                                  nodeColorScheme: n
                                                                    .map(function (e) {
                                                                      return [Math.random(), e]
                                                                    })
                                                                    .sort(function (e, n) {
                                                                      return (
                                                                        (0, a.Z)(e, 1)[0] -
                                                                        (0, a.Z)(n, 1)[0]
                                                                      )
                                                                    })
                                                                    .map(function (e) {
                                                                      var n = (0, a.Z)(e, 2)
                                                                      n[0]
                                                                      return n[1]
                                                                    }),
                                                                },
                                                              ),
                                                            )
                                                          },
                                                        }),
                                                      }),
                                                      (0, l.jsx)(M.u, {
                                                        label: 'Cycle node colors',
                                                        children: (0, l.jsx)(B.h, {
                                                          'aria-label': 'Shift node colors',
                                                          icon: (0, l.jsx)(Z.L, {}),
                                                          size: 'sm',
                                                          variant: 'ghost',
                                                          onClick: function () {
                                                            var e,
                                                              n =
                                                                null !== (e = u.nodeColorScheme) &&
                                                                void 0 !== e
                                                                  ? e
                                                                  : []
                                                            h(
                                                              te(
                                                                te({}, u),
                                                                {},
                                                                {
                                                                  nodeColorScheme: [].concat(
                                                                    (0, s.Z)(n.slice(1, n.length)),
                                                                    [n[0]],
                                                                  ),
                                                                },
                                                              ),
                                                            )
                                                          },
                                                        }),
                                                      }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        closeOnSelect: !1,
                                                        matchWidth: !0,
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            width: 20,
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(q.k, {
                                                              height: 6,
                                                              width: 6,
                                                              flexDirection: 'column',
                                                              flexWrap: 'wrap',
                                                              children: u.nodeColorScheme.map(
                                                                function (e) {
                                                                  return (0, l.jsx)(
                                                                    p.xu,
                                                                    {
                                                                      bgColor: e,
                                                                      flex: '1 1 8px',
                                                                      borderRadius: '2xl',
                                                                    },
                                                                    e,
                                                                  )
                                                                },
                                                              ),
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: (0, l.jsx)(V.__, {
                                                                  width: 500,
                                                                  type: 'checkbox',
                                                                  defaultValue: u.nodeColorScheme,
                                                                  onChange: function (e) {
                                                                    e.length &&
                                                                      h(
                                                                        te(
                                                                          te({}, u),
                                                                          {},
                                                                          { nodeColorScheme: e },
                                                                        ),
                                                                      )
                                                                  },
                                                                  children: w.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.ii,
                                                                      {
                                                                        isChecked:
                                                                          u.nodeColorScheme.some(
                                                                            function (n) {
                                                                              return n === e
                                                                            },
                                                                          ),
                                                                        value: e,
                                                                        isDisabled:
                                                                          1 ===
                                                                            u.nodeColorScheme
                                                                              .length &&
                                                                          u.nodeColorScheme[0] ===
                                                                            e,
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          justifyContent:
                                                                            'space-between',
                                                                          alignItems: 'center',
                                                                          display: 'flex',
                                                                          children: (0, l.jsx)(
                                                                            p.xu,
                                                                            {
                                                                              bgColor: e,
                                                                              borderRadius: 'sm',
                                                                              height: 6,
                                                                              width: 6,
                                                                            },
                                                                          ),
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                }),
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Links' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              children: u.linkColorScheme
                                                                ? (0, l.jsx)(p.xu, {
                                                                    bgColor: u.linkColorScheme,
                                                                    borderRadius: 'sm',
                                                                    height: 6,
                                                                    width: 6,
                                                                  })
                                                                : (0, l.jsx)(q.k, {
                                                                    height: 6,
                                                                    width: 6,
                                                                    flexDirection: 'column',
                                                                    flexWrap: 'wrap',
                                                                    children: u.nodeColorScheme.map(
                                                                      function (e) {
                                                                        return (0, l.jsx)(
                                                                          p.xu,
                                                                          {
                                                                            bgColor: e,
                                                                            flex: '1 1 8px',
                                                                            borderRadius: '2xl',
                                                                          },
                                                                          e,
                                                                        )
                                                                      },
                                                                    ),
                                                                  }),
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(V.sN, {
                                                                    onClick: function () {
                                                                      return h(
                                                                        te(
                                                                          te({}, u),
                                                                          {},
                                                                          { linkColorScheme: '' },
                                                                        ),
                                                                      )
                                                                    },
                                                                    justifyContent: 'space-between',
                                                                    alignItems: 'center',
                                                                    display: 'flex',
                                                                    children: (0, l.jsx)(q.k, {
                                                                      height: 6,
                                                                      width: 6,
                                                                      flexDirection: 'column',
                                                                      flexWrap: 'wrap',
                                                                      children:
                                                                        u.nodeColorScheme.map(
                                                                          function (e) {
                                                                            return (0, l.jsx)(
                                                                              p.xu,
                                                                              {
                                                                                bgColor: e,
                                                                                flex: '1 1 8px',
                                                                                borderRadius: '2xl',
                                                                              },
                                                                              e,
                                                                            )
                                                                          },
                                                                        ),
                                                                    }),
                                                                  }),
                                                                  F.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            te(
                                                                              te({}, u),
                                                                              {},
                                                                              {
                                                                                linkColorScheme: e,
                                                                              },
                                                                            ),
                                                                          )
                                                                        },
                                                                        justifyContent:
                                                                          'space-between',
                                                                        alignItems: 'center',
                                                                        display: 'flex',
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          bgColor: e,
                                                                          borderRadius: 'sm',
                                                                          height: 6,
                                                                          width: 6,
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                ],
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Accent' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: C,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: w.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return k(e)
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        bgColor: e,
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    e,
                                                                  )
                                                                }),
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, {
                                                        children: 'Link Highlight',
                                                      }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.linkHighlight,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          te(
                                                                            te({}, u),
                                                                            {},
                                                                            { linkHighlight: '' },
                                                                          ),
                                                                        )
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    'none',
                                                                  ),
                                                                  w.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            te(
                                                                              te({}, u),
                                                                              {},
                                                                              { linkHighlight: e },
                                                                            ),
                                                                          )
                                                                        },
                                                                        justifyContent:
                                                                          'space-between',
                                                                        alignItems: 'center',
                                                                        display: 'flex',
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          bgColor: e,
                                                                          borderRadius: 'sm',
                                                                          height: 6,
                                                                          width: 6,
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                ],
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, {
                                                        children: 'Node Highlight',
                                                      }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.nodeHighlight,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          te(
                                                                            te({}, u),
                                                                            {},
                                                                            { nodeHighlight: '' },
                                                                          ),
                                                                        )
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    'none',
                                                                  ),
                                                                  w.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            te(
                                                                              te({}, u),
                                                                              {},
                                                                              { nodeHighlight: e },
                                                                            ),
                                                                          )
                                                                        },
                                                                        justifyContent:
                                                                          'space-between',
                                                                        alignItems: 'center',
                                                                        display: 'flex',
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          bgColor: e,
                                                                          borderRadius: 'sm',
                                                                          height: 6,
                                                                          width: 6,
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                ],
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Background' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.backgroundColor,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(V.qy, {
                                                                minWidth: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: F.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          te(
                                                                            te({}, u),
                                                                            {},
                                                                            { backgroundColor: e },
                                                                          ),
                                                                        )
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        bgColor: e,
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    e,
                                                                  )
                                                                }),
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Emacs Node' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.emacsNodeColor,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(V.qy, {
                                                                minWidth: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          te(
                                                                            te({}, u),
                                                                            {},
                                                                            { emacsNodeColor: '' },
                                                                          ),
                                                                        )
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    'none',
                                                                  ),
                                                                  w.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            te(
                                                                              te({}, u),
                                                                              {},
                                                                              { emacsNodeColor: e },
                                                                            ),
                                                                          )
                                                                        },
                                                                        justifyContent:
                                                                          'space-between',
                                                                        alignItems: 'center',
                                                                        display: 'flex',
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          bgColor: e,
                                                                          borderRadius: 'sm',
                                                                          height: 6,
                                                                          width: 6,
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                ],
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                ],
                                              }),
                                            }),
                                          }),
                                        ],
                                      }),
                                    }),
                                    (0, l.jsxs)(_.gC, {
                                      spacing: 2,
                                      justifyContent: 'flex-start',
                                      divider: (0, l.jsx)(_.cX, { borderColor: 'gray.500' }),
                                      align: 'stretch',
                                      paddingLeft: 7,
                                      color: 'gray.800',
                                      children: [
                                        (0, l.jsx)(oe, {
                                          label: 'Node size',
                                          value: u.nodeRel,
                                          onChange: function (e) {
                                            return h(te(te({}, u), {}, { nodeRel: e }))
                                          },
                                        }),
                                        i &&
                                          (0, l.jsxs)(l.Fragment, {
                                            children: [
                                              (0, l.jsx)(oe, {
                                                label: 'Node opacity',
                                                value: u.nodeOpacity,
                                                min: 0,
                                                max: 1,
                                                onChange: function (e) {
                                                  return h(te(te({}, u), {}, { nodeOpacity: e }))
                                                },
                                              }),
                                              (0, l.jsx)(oe, {
                                                label: 'Node resolution',
                                                value: u.nodeResolution,
                                                min: 5,
                                                max: 32,
                                                step: 1,
                                                onChange: function (e) {
                                                  return h(te(te({}, u), {}, { nodeResolution: e }))
                                                },
                                              }),
                                            ],
                                          }),
                                        (0, l.jsx)(oe, {
                                          label: 'Link width',
                                          value: u.linkWidth,
                                          onChange: function (e) {
                                            return h(te(te({}, u), {}, { linkWidth: e }))
                                          },
                                        }),
                                        i &&
                                          (0, l.jsx)(oe, {
                                            label: 'Link opacity',
                                            min: 0,
                                            max: 1,
                                            value: u.linkOpacity,
                                            onChange: function (e) {
                                              return h(te(te({}, u), {}, { linkOpacity: e }))
                                            },
                                          }),
                                        (0, l.jsxs)(p.xu, {
                                          children: [
                                            (0, l.jsxs)(q.k, {
                                              alignItems: 'center',
                                              justifyContent: 'space-between',
                                              children: [
                                                (0, l.jsx)(U.x, { children: 'Labels' }),
                                                (0, l.jsxs)(V.v2, {
                                                  placement: 'right',
                                                  children: [
                                                    (0, l.jsx)(V.j2, {
                                                      as: W.z,
                                                      colorScheme: '',
                                                      color: 'black',
                                                      rightIcon: (0, l.jsx)(P.v, {}),
                                                      children: u.labels
                                                        ? u.labels < 2
                                                          ? 'On Highlight'
                                                          : 'Always'
                                                        : 'Never',
                                                    }),
                                                    (0, l.jsxs)(K.h, {
                                                      children: [
                                                        ' ',
                                                        (0, l.jsxs)(V.qy, {
                                                          zIndex: 'popover',
                                                          bgColor: 'gray.200',
                                                          children: [
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  te(te({}, u), {}, { labels: 0 }),
                                                                )
                                                              },
                                                              children: 'Never',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  te(te({}, u), {}, { labels: 1 }),
                                                                )
                                                              },
                                                              children: 'On Highlight',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  te(te({}, u), {}, { labels: 2 }),
                                                                )
                                                              },
                                                              children: 'Always',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  te(te({}, u), {}, { labels: 3 }),
                                                                )
                                                              },
                                                              children: 'Always (even in 3D)',
                                                            }),
                                                          ],
                                                        }),
                                                      ],
                                                    }),
                                                  ],
                                                }),
                                              ],
                                            }),
                                            (0, l.jsx)(J.U, {
                                              in: u.labels > 0,
                                              animateOpacity: !0,
                                              children: (0, l.jsxs)(_.gC, {
                                                spacing: 1,
                                                justifyContent: 'flex-start',
                                                divider: (0, l.jsx)(_.cX, {
                                                  borderColor: 'gray.400',
                                                }),
                                                align: 'stretch',
                                                paddingLeft: 2,
                                                color: 'gray.800',
                                                children: [
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Text' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        matchWidth: !0,
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            color: 'black',
                                                            colorScheme: '',
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.labelTextColor,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bg: 'gray.200',
                                                                children: F.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          te(
                                                                            te({}, u),
                                                                            {},
                                                                            { labelTextColor: e },
                                                                          ),
                                                                        )
                                                                      },
                                                                      children: (0, l.jsx)(p.xu, {
                                                                        bgColor: e,
                                                                        borderRadius: 'sm',
                                                                        height: 6,
                                                                        width: 6,
                                                                      }),
                                                                    },
                                                                    e,
                                                                  )
                                                                }),
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsxs)(q.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(U.x, { children: 'Background' }),
                                                      (0, l.jsxs)(V.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(V.j2, {
                                                            as: W.z,
                                                            rightIcon: (0, l.jsx)(P.v, {}),
                                                            color: 'black',
                                                            colorScheme: '',
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: u.labelBackgroundColor,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(K.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(V.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bg: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(V.sN, {
                                                                    onClick: function () {
                                                                      return h(
                                                                        te(
                                                                          te({}, u),
                                                                          {},
                                                                          {
                                                                            labelBackgroundColor:
                                                                              '',
                                                                          },
                                                                        ),
                                                                      )
                                                                    },
                                                                    justifyContent: 'space-between',
                                                                    alignItems: 'center',
                                                                    display: 'flex',
                                                                    children: 'None',
                                                                  }),
                                                                  F.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            te(
                                                                              te({}, u),
                                                                              {},
                                                                              {
                                                                                labelBackgroundColor:
                                                                                  e,
                                                                              },
                                                                            ),
                                                                          )
                                                                        },
                                                                        justifyContent:
                                                                          'space-between',
                                                                        alignItems: 'center',
                                                                        display: 'flex',
                                                                        children: (0, l.jsx)(p.xu, {
                                                                          bgColor: e,
                                                                          borderRadius: 'sm',
                                                                          height: 6,
                                                                          width: 6,
                                                                        }),
                                                                      },
                                                                      e,
                                                                    )
                                                                  }),
                                                                ],
                                                              }),
                                                            ],
                                                          }),
                                                        ],
                                                      }),
                                                    ],
                                                  }),
                                                  (0, l.jsx)(J.U, {
                                                    in: !!u.labelBackgroundColor,
                                                    animateOpacity: !0,
                                                    children: (0, l.jsx)(p.xu, {
                                                      paddingTop: 2,
                                                      children: (0, l.jsx)(oe, {
                                                        label: 'Background opacity',
                                                        value: u.labelBackgroundOpacity,
                                                        onChange: function (e) {
                                                          console.log(u.labelBackgroundOpacity),
                                                            h(
                                                              te(
                                                                te({}, u),
                                                                {},
                                                                { labelBackgroundOpacity: e },
                                                              ),
                                                            )
                                                        },
                                                        min: 0,
                                                        max: 1,
                                                        step: 0.01,
                                                      }),
                                                    }),
                                                  }),
                                                  (0, l.jsx)(J.U, {
                                                    in: u.labels > 1,
                                                    animateOpacity: !0,
                                                    children: (0, l.jsx)(p.xu, {
                                                      paddingTop: 2,
                                                      children: (0, l.jsx)(oe, {
                                                        label: 'Label Appearance Scale',
                                                        value: 5 * u.labelScale,
                                                        onChange: function (e) {
                                                          return h(
                                                            te(
                                                              te({}, u),
                                                              {},
                                                              { labelScale: e / 5 },
                                                            ),
                                                          )
                                                        },
                                                      }),
                                                    }),
                                                  }),
                                                ],
                                              }),
                                            }),
                                          ],
                                        }),
                                        (0, l.jsxs)(le, {
                                          label: 'Directional Particles',
                                          value: u.particles,
                                          onChange: function () {
                                            return h(te(te({}, u), {}, { particles: !u.particles }))
                                          },
                                          children: [
                                            (0, l.jsx)(oe, {
                                              label: 'Particle Number',
                                              value: u.particlesNumber,
                                              max: 5,
                                              step: 1,
                                              onChange: function (e) {
                                                return h(te(te({}, u), {}, { particlesNumber: e }))
                                              },
                                            }),
                                            (0, l.jsx)(oe, {
                                              label: 'Particle Size',
                                              value: u.particlesWidth,
                                              onChange: function (e) {
                                                return h(te(te({}, u), {}, { particlesWidth: e }))
                                              },
                                            }),
                                          ],
                                        }),
                                        (0, l.jsx)(le, {
                                          label: 'Highlight',
                                          onChange: function () {
                                            return h(te(te({}, u), {}, { highlight: !u.highlight }))
                                          },
                                          value: u.highlight,
                                          children: (0, l.jsxs)(_.gC, {
                                            spacing: 1,
                                            justifyContent: 'flex-start',
                                            divider: (0, l.jsx)(_.cX, { borderColor: 'gray.400' }),
                                            align: 'stretch',
                                            paddingLeft: 0,
                                            children: [
                                              (0, l.jsx)(oe, {
                                                label: 'Highlight Link Thickness',
                                                value: u.highlightLinkSize,
                                                onChange: function (e) {
                                                  return h(
                                                    te(te({}, u), {}, { highlightLinkSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsx)(oe, {
                                                label: 'Highlight Node Size',
                                                value: u.highlightNodeSize,
                                                onChange: function (e) {
                                                  return h(
                                                    te(te({}, u), {}, { highlightNodeSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsxs)(le, {
                                                label: 'Highlight Animation',
                                                onChange: function () {
                                                  h(
                                                    te(
                                                      te({}, u),
                                                      {},
                                                      { highlightAnim: !u.highlightAnim },
                                                    ),
                                                  )
                                                },
                                                value: u.highlightAnim,
                                                children: [
                                                  (0, l.jsx)(oe, {
                                                    label: 'Animation speed',
                                                    onChange: function (e) {
                                                      return h(
                                                        te(te({}, u), {}, { animationSpeed: e }),
                                                      )
                                                    },
                                                    value: u.animationSpeed,
                                                    infoText:
                                                      'Slower speed has a chance of being buggy',
                                                    min: 50,
                                                    max: 1e3,
                                                    step: 10,
                                                  }),
                                                  (0, l.jsx)(Y.Ph, {
                                                    placeholder: u.algorithmName,
                                                    onChange: function (e) {
                                                      h(
                                                        te(
                                                          te({}, u),
                                                          {},
                                                          { algorithmName: e.target.value },
                                                        ),
                                                      )
                                                    },
                                                    children: u.algorithmOptions.map(function (e) {
                                                      return (0,
                                                      l.jsx)('option', { value: e, children: e }, e)
                                                    }),
                                                  }),
                                                ],
                                              }),
                                            ],
                                          }),
                                        }),
                                      ],
                                    }),
                                  ],
                                }),
                              }),
                            ],
                          }),
                          (0, l.jsxs)(X.Qd, {
                            children: [
                              (0, l.jsxs)(X.KF, {
                                children: [
                                  (0, l.jsx)(X.XE, { marginRight: 2 }),
                                  (0, l.jsx)(A.X, { size: 'sm', children: 'Behavior' }),
                                ],
                              }),
                              (0, l.jsx)(X.Hk, {
                                children: (0, l.jsxs)(_.gC, {
                                  spacing: 2,
                                  justifyContent: 'flex-start',
                                  divider: (0, l.jsx)(_.cX, { borderColor: 'gray.500' }),
                                  align: 'stretch',
                                  paddingLeft: 7,
                                  color: 'gray.800',
                                  children: [
                                    (0, l.jsxs)(q.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsxs)(q.k, {
                                          children: [
                                            (0, l.jsx)(U.x, { children: 'Expand Node' }),
                                            (0, l.jsx)(re, {
                                              infoText:
                                                'View only the node and its direct neighbors',
                                            }),
                                          ],
                                        }),
                                        (0, l.jsxs)(V.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(V.j2, {
                                              as: W.z,
                                              rightIcon: (0, l.jsx)(P.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(U.x, {
                                                children:
                                                  g.local[0].toUpperCase() + g.local.slice(1),
                                              }),
                                            }),
                                            (0, l.jsxs)(K.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(V.qy, {
                                                  zIndex: 'popover',
                                                  bgColor: 'gray.200',
                                                  children: [
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(te(te({}, g), {}, { local: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { local: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { local: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { local: 'right' }),
                                                        )
                                                      },
                                                      children: 'Right Click',
                                                    }),
                                                  ],
                                                }),
                                              ],
                                            }),
                                          ],
                                        }),
                                      ],
                                    }),
                                    (0, l.jsxs)(q.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(U.x, { children: 'Open in Emacs' }),
                                        (0, l.jsxs)(V.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(V.j2, {
                                              as: W.z,
                                              rightIcon: (0, l.jsx)(P.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(U.x, {
                                                children:
                                                  g.follow[0].toUpperCase() + g.follow.slice(1),
                                              }),
                                            }),
                                            (0, l.jsxs)(K.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(V.qy, {
                                                  bgColor: 'gray.200',
                                                  zIndex: 'popover',
                                                  children: [
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(te(te({}, g), {}, { follow: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { follow: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { follow: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          te(te({}, g), {}, { follow: 'right' }),
                                                        )
                                                      },
                                                      children: 'Right Click',
                                                    }),
                                                  ],
                                                }),
                                              ],
                                            }),
                                          ],
                                        }),
                                      ],
                                    }),
                                    (0, l.jsxs)(q.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(U.x, { children: 'Follow Emacs by...' }),
                                        (0, l.jsxs)(V.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(V.j2, {
                                              as: W.z,
                                              rightIcon: (0, l.jsx)(P.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(U.x, {
                                                children:
                                                  f.follow[0].toUpperCase() + f.follow.slice(1),
                                              }),
                                            }),
                                            (0, l.jsxs)(K.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(V.qy, {
                                                  bgColor: 'gray.200',
                                                  zIndex: 'popover',
                                                  children: [
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return j(
                                                          te(te({}, f), {}, { follow: 'local' }),
                                                        )
                                                      },
                                                      children: 'Opening the local graph',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return j(
                                                          te(te({}, f), {}, { follow: 'zoom' }),
                                                        )
                                                      },
                                                      children: 'Zooming to the current node',
                                                    }),
                                                  ],
                                                }),
                                              ],
                                            }),
                                          ],
                                        }),
                                      ],
                                    }),
                                    (0, l.jsx)(oe, {
                                      label: 'Zoom speed',
                                      value: f.zoomSpeed,
                                      min: 0,
                                      max: 4e3,
                                      step: 100,
                                      onChange: function (e) {
                                        return j(te(te({}, f), {}, { zoomSpeed: e }))
                                      },
                                    }),
                                    (0, l.jsx)(oe, {
                                      label: 'Zoom padding',
                                      value: f.zoomPadding,
                                      min: 0,
                                      max: 400,
                                      step: 1,
                                      onChange: function (e) {
                                        return j(te(te({}, f), {}, { zoomPadding: e }))
                                      },
                                      infoText:
                                        'How much to zoom out to accomodate all nodes when changing the view.',
                                    }),
                                  ],
                                }),
                              }),
                            ],
                          }),
                        ],
                      }),
                    }),
                  ],
                }),
              }),
            ],
          })
        },
        re = function (e) {
          var n = e.infoText
          return (0, l.jsx)(p.xu, {
            paddingLeft: '1',
            children: (0, l.jsx)(M.u, {
              label: n,
              placement: 'top',
              color: 'gray.100',
              bg: 'gray.800',
              hasArrow: !0,
              children: (0, l.jsx)(F.h, {}),
            }),
          })
        },
        oe = function (e) {
          var n = e.min,
            t = void 0 === n ? 0 : n,
            i = e.max,
            r = void 0 === i ? 10 : i,
            o = e.step,
            s = void 0 === o ? 0.1 : o,
            c = e.value,
            a = void 0 === c ? 1 : c,
            u = (0, R.Z)(e, ['min', 'max', 'step', 'value']),
            h = u.onChange,
            g = u.label,
            x = u.infoText,
            f = (0, d.useContext)(ee.N).highlightColor
          return (0, l.jsxs)(p.xu, {
            children: [
              (0, l.jsxs)(p.xu, {
                display: 'flex',
                alignItems: 'flex-end',
                children: [(0, l.jsx)(U.x, { children: g }), x && (0, l.jsx)(re, { infoText: x })],
              }),
              (0, l.jsxs)(G.iR, {
                value: a,
                onChange: h,
                min: t,
                max: r,
                step: s,
                children: [
                  (0, l.jsx)(G.Uj, { children: (0, l.jsx)(G.Ms, {}) }),
                  (0, l.jsx)(M.u, {
                    bg: f,
                    label: a.toFixed(1),
                    children: (0, l.jsx)(G.gs, { bg: 'white' }),
                  }),
                ],
              }),
            ],
          })
        },
        le = function (e) {
          var n = e.value,
            t = e.onChange,
            i = e.label,
            r = e.infoText,
            o = e.children
          return (0, l.jsxs)(p.xu, {
            paddingTop: 2,
            children: [
              (0, l.jsxs)(p.xu, {
                display: 'flex',
                justifyContent: 'space-between',
                paddingBottom: 2,
                children: [
                  (0, l.jsxs)(p.xu, {
                    display: 'flex',
                    alignItems: 'center',
                    children: [
                      (0, l.jsx)(U.x, { children: i }),
                      r && (0, l.jsx)(re, { infoText: r }),
                    ],
                  }),
                  (0, l.jsx)(Q.r, { isChecked: !!n, onChange: t }),
                ],
              }),
              (0, l.jsx)(J.U, {
                in: !!n,
                animateOpacity: !0,
                children: (0, l.jsx)(p.xu, {
                  paddingLeft: 4,
                  paddingTop: 2,
                  paddingBottom: 2,
                  children: o,
                }),
              }),
            ],
          })
        },
        se = t(1122),
        ce = t(2003)
      function ae(e, n) {
        var t = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var i = Object.getOwnPropertySymbols(e)
          n &&
            (i = i.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            t.push.apply(t, i)
        }
        return t
      }
      function de(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? ae(Object(t), !0).forEach(function (n) {
                ;(0, c.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : ae(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var ue = t.e(4).then(t.bind(t, 7004)),
        he = t.g.window ? t(1957).f$ : null,
        ge = t.g.window ? t(1957).s6 : null
      function xe() {
        var e = (0, d.useState)(!1),
          n = e[0],
          t = e[1]
        return (
          (0, d.useEffect)(function () {
            t(!0)
          }, []),
          n ? (0, l.jsx)(fe, {}) : null
        )
      }
      function fe() {
        var e = u('physics', S),
          n = (0, a.Z)(e, 2),
          t = n[0],
          i = n[1],
          r = u('filter', I),
          o = (0, a.Z)(r, 2),
          h = o[0],
          g = o[1],
          x = u('visuals', O),
          f = (0, a.Z)(x, 2),
          j = f[0],
          m = f[1],
          b = (0, d.useState)(null),
          v = b[0],
          y = b[1],
          C = (0, d.useState)(null),
          k = C[0],
          w = C[1],
          R = u('behavior', N),
          T = (0, a.Z)(R, 2),
          D = T[0],
          H = T[1],
          E = u('mouse', z),
          Z = (0, a.Z)(E, 2),
          P = Z[0],
          F = Z[1],
          L = (0, d.useRef)({}),
          B = (0, d.useRef)({}),
          M = (0, d.useContext)(ee.N).setEmacsTheme,
          W = (0, d.useState)(!1),
          X = W[0],
          A = W[1],
          _ = (0, d.useState)({ nodeIds: [] }),
          q = _[0],
          U = _[1],
          Q = (0, d.useRef)({ nodeIds: [] }),
          V = (0, d.useRef)(N)
        V.current = D
        var K = (0, d.useRef)(null),
          J = (0, d.useRef)(null)
        Q.current = q
        var Y = function (e, n) {
          var t,
            i = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : 2e3,
            r = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : 200,
            o = K.current,
            l = Q.current,
            c = V.current,
            a = null !== (t = B.current[n]) && void 0 !== t ? t : [],
            d = Object.fromEntries(
              [n]
                .concat(
                  (0, s.Z)(
                    a.flatMap(function (e) {
                      return [e.source, e.target]
                    }),
                  ),
                )
                .map(function (e) {
                  return [e, {}]
                }),
            )
          return 'zoom' === e
            ? (console.log(l),
              l.nodeIds.length &&
                (console.log('emptying'), console.log('scope ' + l.nodeIds), U({ nodeIds: [] })),
              void setTimeout(function () {
                return o.zoomToFit(i, r, function (e) {
                  return d[e.id]
                })
              }, 50))
            : l.nodeIds.length
            ? 'add' !== c.localSame
              ? (U({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.zoomToFit(i, r, function (e) {
                    return d[e.id]
                  })
                }, 50))
              : l.nodeIds.includes(n) &&
                l.nodeIds.some(function (e) {
                  return d[e]
                })
              ? (U(function (e) {
                  return de(de({}, e), {}, { nodeIds: [].concat((0, s.Z)(e.nodeIds), [n]) })
                }),
                void setTimeout(function () {
                  return o.zoomToFit(i, r, function (e) {
                    return d[e.id]
                  })
                }, 50))
              : (U({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.zoomToFit(i, r, function (e) {
                    return d[e.id]
                  })
                }, 50))
            : (U({ nodeIds: [n] }),
              void setTimeout(function () {
                o.zoomToFit(i, r, function (e) {
                  return d[e.id]
                })
              }, 50))
        }
        return (
          (0, d.useEffect)(function () {
            ;(J.current = new ce.Z('ws://localhost:35903')),
              J.current.addEventListener('open', function (e) {
                console.log('Connection with Emacs established')
              }),
              J.current.addEventListener('message', function (e) {
                K.current
                var n = V.current,
                  t = JSON.parse(e.data)
                switch (t.type) {
                  case 'graphdata':
                    return (function (e) {
                      var n = e.nodes.reduce(function (e, n) {
                          var t
                          return de(
                            de({}, e),
                            {},
                            (0, c.Z)(
                              {},
                              n.file,
                              [].concat(
                                (0, s.Z)(null !== (t = e[n.file]) && void 0 !== t ? t : []),
                                [n],
                              ),
                            ),
                          )
                        }, {}),
                        t = Object.keys(n).flatMap(function (e) {
                          var t,
                            i = null !== (t = n[e]) && void 0 !== t ? t : [],
                            r = i.find(function (e) {
                              return 0 === e.level
                            }),
                            o = i.filter(function (e) {
                              return 0 !== e.level
                            })
                          return r
                            ? o.map(function (e) {
                                return { source: e.id, target: r.id, type: 'parent' }
                              })
                            : []
                        })
                      L.current = Object.fromEntries(
                        e.nodes.map(function (e) {
                          return [e.id, e]
                        }),
                      )
                      var i = [].concat((0, s.Z)(e.links), (0, s.Z)(t))
                      B.current = i.reduce(function (e, n) {
                        var t, i, r
                        return de(
                          de({}, e),
                          {},
                          ((r = {}),
                          (0, c.Z)(
                            r,
                            n.source,
                            [].concat(
                              (0, s.Z)(null !== (t = e[n.source]) && void 0 !== t ? t : []),
                              [n],
                            ),
                          ),
                          (0, c.Z)(
                            r,
                            n.target,
                            [].concat(
                              (0, s.Z)(null !== (i = e[n.target]) && void 0 !== i ? i : []),
                              [n],
                            ),
                          ),
                          r),
                        )
                      }, {})
                      var r = de(de({}, e), {}, { links: i }),
                        o = JSON.parse(JSON.stringify(r))
                      y(o)
                    })(t.data)
                  case 'theme':
                    return M(t.data)
                  case 'command':
                    switch (t.data.commandName) {
                      case 'local':
                        var i = D.zoomSpeed,
                          r = D.zoomPadding
                        Y('local', t.data.id, i, r), w(t.data.id)
                        break
                      case 'zoom':
                        var o,
                          l,
                          a =
                            (null === t || void 0 === t || null === (o = t.data) || void 0 === o
                              ? void 0
                              : o.speed) || n.zoomSpeed,
                          d =
                            (null === t || void 0 === t || null === (l = t.data) || void 0 === l
                              ? void 0
                              : l.padding) || n.zoomPadding
                        Y('zoom', t.data.id, a, d), w(t.data.id)
                        break
                      case 'follow':
                        Y(n.follow, t.data.id, n.zoomSpeed, n.zoomPadding), w(t.data.id)
                        break
                      default:
                        return console.error('unknown message type', t.type)
                    }
                }
              })
          }, []),
          v
            ? (0, l.jsxs)(p.xu, {
                display: 'flex',
                alignItems: 'flex-start',
                flexDirection: 'row',
                height: '100%',
                children: [
                  (0, l.jsx)(
                    ie,
                    de(
                      {},
                      {
                        physics: t,
                        setPhysics: i,
                        threeDim: X,
                        setThreeDim: A,
                        filter: h,
                        setFilter: g,
                        visuals: j,
                        setVisuals: m,
                        mouse: P,
                        setMouse: F,
                        behavior: D,
                        setBehavior: H,
                      },
                    ),
                  ),
                  (0, l.jsx)(p.xu, {
                    position: 'absolute',
                    alignItems: 'top',
                    children: (0, l.jsx)(
                      je,
                      de(
                        {
                          ref: K,
                          nodeById: L.current,
                          linksByNodeId: B.current,
                          webSocket: J.current,
                        },
                        {
                          physics: t,
                          graphData: v,
                          threeDim: X,
                          emacsNodeId: k,
                          filter: h,
                          visuals: j,
                          behavior: D,
                          mouse: P,
                          scope: q,
                          setScope: U,
                        },
                      ),
                    ),
                  }),
                ],
              })
            : null
        )
      }
      var je = (0, d.forwardRef)(function (e, n) {
        var t = e.physics,
          i = e.graphData,
          c = e.threeDim,
          u = e.linksByNodeId,
          h = e.filter,
          p = e.emacsNodeId,
          m = e.nodeById,
          b = e.visuals,
          v = (e.behavior, e.mouse),
          y = e.scope,
          C = e.setScope,
          k = e.webSocket,
          S = (0, x.iP)(),
          I = (0, a.Z)(S, 2),
          O = I[0],
          N = I[1],
          z = (0, d.useState)(null),
          R = z[0],
          T = z[1],
          D = (0, j.useTheme)(),
          H = (0, d.useContext)(ee.N).emacsTheme,
          E = function (e, n) {
            switch (e) {
              case v.local:
                if (y.nodeIds.includes(n.id)) break
                C(function (e) {
                  return de(de({}, e), {}, { nodeIds: [].concat((0, s.Z)(e.nodeIds), [n.id]) })
                })
                break
              case v.follow:
                k.send(n.id)
            }
          },
          Z = (0, d.useRef)(null)
        ;(0, d.useEffect)(
          function () {
            p && T(m[p])
          },
          [p],
        ),
          (Z.current = R)
        var P = (0, d.useMemo)(
            function () {
              if (!Z.current) return {}
              var e = u[Z.current.id]
              return e
                ? Object.fromEntries(
                    [Z.current.id]
                      .concat(
                        (0, s.Z)(
                          e.flatMap(function (e) {
                            return [e.source, e.target]
                          }),
                        ),
                      )
                      .map(function (e) {
                        return [e, {}]
                      }),
                  )
                : {}
            },
            [Z.current, u],
          ),
          F = (0, d.useMemo)(
            function () {
              var e = i.nodes.filter(function (e) {
                  var n,
                    t = null !== (n = u[e.id]) && void 0 !== n ? n : []
                  return (
                    !h.orphans ||
                    (h.parents
                      ? 0 !== t.length
                      : 0 !== t.length &&
                        t.some(function (e) {
                          return !['parent', 'cite', 'ref'].includes(e.type)
                        }))
                  )
                }),
                n = i.links.filter(function (e) {
                  var n = e
                  return 'cite' !== n.type && (h.parents || 'parent' !== n.type)
                }),
                t = e.filter(function (e) {
                  var n,
                    t = null !== (n = u[e.id]) && void 0 !== n ? n : []
                  return (
                    y.nodeIds.includes(e.id) ||
                    t.some(function (e) {
                      return y.nodeIds.includes(e.source) || y.nodeIds.includes(e.target)
                    })
                  )
                }),
                r = t.map(function (e) {
                  return e.id
                }),
                o = n.filter(function (e) {
                  var n = 'object' === typeof e.source ? e.source.id : e.source,
                    t = 'object' === typeof e.target ? e.target.id : e.target
                  return r.includes(n) && r.includes(t)
                })
              return 0 === y.nodeIds.length ? { nodes: e, links: n } : { nodes: t, links: o }
            },
            [h, y, i],
          )
        ;(0, d.useEffect)(function () {
          ;(0, o.Z)(
            r().mark(function e() {
              var i, o
              return r().wrap(function (e) {
                for (;;)
                  switch ((e.prev = e.next)) {
                    case 0:
                      return (i = n.current), (e.next = 3), ue
                    case 3:
                      ;(o = e.sent),
                        t.gravityOn
                          ? (i.d3Force('x', o.forceX().strength(t.gravity)),
                            i.d3Force('y', o.forceY().strength(t.gravity)),
                            c && i.d3Force('z', o.forceZ().strength(t.gravity)))
                          : (i.d3Force('x', null), i.d3Force('y', null), c && i.d3Force('z', null)),
                        t.centering
                          ? i.d3Force('center', o.forceCenter().strength(t.centeringStrength))
                          : i.d3Force('center', null),
                        t.linkStrength && i.d3Force('link').strength(t.linkStrength),
                        t.linkIts && i.d3Force('link').iterations(t.linkIts),
                        t.charge && i.d3Force('charge').strength(t.charge),
                        i.d3Force(
                          'collide',
                          t.collision ? o.forceCollide().radius(t.collisionStrength) : null,
                        )
                    case 10:
                    case 'end':
                      return e.stop()
                  }
              }, e)
            }),
          )()
        }),
          (0, d.useEffect)(
            function () {
              var e
              null === (e = n.current) || void 0 === e || e.d3ReheatSimulation()
            },
            [t],
          )
        var L = (0, d.useRef)(0),
          B = (0, d.useState)(1),
          M = B[0],
          W = B[1],
          X = (0, f._7)(
            function (e) {
              return W(e)
            },
            { duration: b.animationSpeed, algorithm: w[b.algorithmName] },
          ),
          A = (0, a.Z)(X, 2),
          _ = A[0],
          q = A[1],
          U = (0, f._7)(
            function (e) {
              return W(Math.min(M, -1 * (e - 1)))
            },
            { duration: b.animationSpeed, algorithm: w[b.algorithmName] },
          ),
          Q = (0, a.Z)(U, 2),
          V = Q[0],
          K = Q[1],
          J = (0, d.useRef)(null)
        ;(0, d.useEffect)(
          function () {
            if ((R && (J.current = R), !b.highlightAnim)) return W(R ? 1 : 0)
            R ? _() : (q(), M > 0.5 ? V() : W(0))
          },
          [R],
        )
        var Y = function (e) {
            if (D)
              return e.split('.').reduce(function (e, n) {
                return e[n]
              }, D.colors)
          },
          G = (0, d.useMemo)(
            function () {
              var e = b.nodeColorScheme.concat(
                b.linkColorScheme || [],
                b.linkHighlight || [],
                b.nodeHighlight || [],
              )
              return Object.fromEntries(
                e.map(function (n) {
                  var t = Y(n),
                    i = e.map(function (e) {
                      return [e, g.Z(t, Y(e))]
                    })
                  return [n, Object.fromEntries(i)]
                }),
              )
            },
            [b.nodeColorScheme, b.linkHighlight, b.nodeHighlight, b.linkColorScheme, H],
          ),
          $ = (0, d.useMemo)(
            function () {
              var e,
                n,
                t,
                i =
                  null !== (e = u[null === (n = J.current) || void 0 === n ? void 0 : n.id]) &&
                  void 0 !== e
                    ? e
                    : []
              return Object.fromEntries(
                [null === (t = J.current) || void 0 === t ? void 0 : t.id]
                  .concat(
                    (0, s.Z)(
                      i.flatMap(function (e) {
                        return [e.source, e.target]
                      }),
                    ),
                  )
                  .map(function (e) {
                    return [e, {}]
                  }),
              )
            },
            [JSON.stringify(R), J.current],
          ),
          ne = function (e) {
            var n,
              t,
              i,
              r,
              o,
              l,
              s =
                null !== (n = null === (t = u[e]) || void 0 === t ? void 0 : t.length) &&
                void 0 !== n
                  ? n
                  : 0,
              c = s
                ? null === (i = u[e]) || void 0 === i
                  ? void 0
                  : i.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                : 0,
              a = h.parents ? s : s - c
            return b.nodeColorScheme[
              ((r = a), (o = 0), (l = b.nodeColorScheme.length - 1), Math.min(Math.max(r, o), l))
            ]
          },
          te = function (e, n) {
            return u[e] > u[n] ? ne(e) : ne(n)
          },
          ie = function (e, n) {
            return (
              'rgba(' +
              (e = e.replace('#', ''))
                .match(new RegExp('(.{' + e.length / 3 + '})', 'g'))
                .map(function (n) {
                  return parseInt(e.length % 2 ? n + n : n, 16)
                })
                .concat(isFinite(n) ? n : 1)
                .join(',') +
              ')'
            )
          },
          re = (0, d.useMemo)(
            function () {
              return Y(b.labelTextColor)
            },
            [b.labelTextColor, H],
          ),
          oe = (0, d.useMemo)(
            function () {
              return Y(b.labelBackgroundColor)
            },
            [b.labelBackgroundColor, H],
          ),
          le = {
            graphData: F,
            width: O,
            height: N,
            backgroundColor: D.colors.gray[b.backgroundColor],
            nodeLabel: function (e) {
              return e.title
            },
            nodeColor: function (e) {
              return (function (e) {
                var n = P[e.id] || $[e.id]
                return b.emacsNodeColor && e.id === p
                  ? Y(b.emacsNodeColor)
                  : n && b.nodeHighlight
                  ? G[ne(e.id)][b.nodeHighlight](M)
                  : Y(ne(e.id))
              })(e)
            },
            nodeRelSize: b.nodeRel,
            nodeVal: function (e) {
              var n,
                t = null !== (n = u[e.id]) && void 0 !== n ? n : [],
                i = t.length
                  ? t.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                  : 0
              return (
                (3 + t.length - (h.parents ? 0 : i)) *
                (P[e.id] || $[e.id] ? 1 + M * (b.highlightNodeSize - 1) : 1)
              )
            },
            nodeCanvasObject: function (e, n, t) {
              if (e && b.labels) {
                var i = $[e.id]
                if (!(t <= b.labelScale || 1 === b.labels) || P[e.id] || i) {
                  var r = e.title,
                    o = r.substring(0, Math.min(r.length, 40)),
                    l = 12 / t,
                    c = [1.1 * n.measureText(o).width, l].map(function (e) {
                      return e + 0.5 * l
                    }),
                    a = Math.min((3 * (t - b.labelScale)) / b.labelScale, 1),
                    d = function () {
                      return 1 === b.labels || t <= b.labelScale
                        ? M
                        : P[e.id] || $[e.id]
                        ? Math.max(a, M)
                        : 1 * a * (-1 * (0.5 * M - 1))
                    }
                  if (b.labelBackgroundColor && b.labelBackgroundOpacity) {
                    var u = d() * b.labelBackgroundOpacity,
                      h = ie(oe, u)
                    ;(n.fillStyle = h),
                      n.fillRect.apply(n, [e.x - c[0] / 2, e.y - c[1] / 2].concat((0, s.Z)(c)))
                  }
                  var g = d()
                  ;(n.textAlign = 'center'), (n.textBaseline = 'middle')
                  var x = ie(re, g)
                  ;(n.fillStyle = x),
                    (n.font = ''.concat(l, 'px Sans-Serif')),
                    n.fillText(o, e.x, e.y)
                }
              }
            },
            nodeCanvasObjectMode: function () {
              return 'after'
            },
            linkDirectionalParticles: b.particles ? b.particlesNumber : void 0,
            linkColor: function (e) {
              var n = 'object' === typeof e.source ? e.source.id : e.source,
                t = 'object' === typeof e.target ? e.target.id : e.target,
                i = pe(e, Z.current),
                r = pe(e, J.current)
              return (function (e, n, t) {
                if (!b.linkHighlight && !b.linkColorScheme && !t) {
                  var i = te(e, n)
                  return Y(i)
                }
                if (!t && !b.linkColorScheme) {
                  var r = te(e, n)
                  return Y(r)
                }
                if (!t) return Y(b.linkColorScheme)
                if (!b.linkHighlight && !b.linkColorScheme) {
                  var o = te(e, n)
                  return Y(o)
                }
                return b.linkHighlight
                  ? b.linkColorScheme
                    ? G[b.linkColorScheme][b.linkHighlight](M)
                    : G[te(e, n)][b.linkHighlight](M)
                  : Y(b.linkColorScheme)
              })(n, t, i || r)
            },
            linkWidth: function (e) {
              var n = pe(e, Z.current),
                t = pe(e, J.current)
              return n || t ? b.linkWidth * (1 + M * (b.highlightLinkSize - 1)) : b.linkWidth
            },
            linkDirectionalParticleWidth: b.particlesWidth,
            d3AlphaDecay: t.alphaDecay,
            d3AlphaMin: t.alphaMin,
            d3VelocityDecay: t.velocityDecay,
            onNodeClick: function (e, n) {
              var t = n.timeStamp - L.current < 400
              return (L.current = n.timeStamp), E(t ? 'double' : 'click', e)
            },
            onBackgroundClick: function () {
              T(null),
                0 !== y.nodeIds.length &&
                  C(function (e) {
                    return de(de({}, e), {}, { nodeIds: [] })
                  })
            },
            onNodeHover: function (e) {
              b.highlight && (R || (K(), W(0)), T(e))
            },
            onNodeRightClick: function (e) {
              E('right', e)
            },
          }
        return (0, l.jsx)('div', {
          children: c
            ? (0, l.jsx)(
                ge,
                de(
                  de({ ref: n }, le),
                  {},
                  {
                    nodeThreeObjectExtend: !0,
                    backgroundColor: D.colors.white,
                    nodeOpacity: b.nodeOpacity,
                    nodeResolution: b.nodeResolution,
                    linkOpacity: b.linkOpacity,
                    nodeThreeObject: function (e) {
                      if (b.labels && (!(b.labels < 3) || P[e.id])) {
                        var n = new se.Z(e.title.substring(0, 40))
                        return (
                          (n.color = Y(b.labelTextColor)),
                          (n.backgroundColor = Y(b.labelBackgroundColor)),
                          (n.padding = 2),
                          (n.textHeight = 8),
                          n
                        )
                      }
                    },
                  },
                ),
              )
            : (0, l.jsx)(he, de({ ref: n }, le)),
        })
      })
      function pe(e, n) {
        return (
          e.source.id === (null === n || void 0 === n ? void 0 : n.id) ||
          e.target.id === (null === n || void 0 === n ? void 0 : n.id)
        )
      }
    },
    5301: function (e, n, t) {
      ;(window.__NEXT_P = window.__NEXT_P || []).push([
        '/',
        function () {
          return t(374)
        },
      ])
    },
  },
  function (e) {
    e.O(0, [774, 737, 446, 906, 888, 179], function () {
      return (n = 5301), e((e.s = n))
      var n
    })
    var n = e.O()
    _N_E = n
  },
])
