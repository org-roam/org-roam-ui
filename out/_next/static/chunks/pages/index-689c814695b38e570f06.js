;(self.webpackChunk_N_E = self.webpackChunk_N_E || []).push([
  [405],
  {
    374: function (e, n, t) {
      'use strict'
      t.r(n),
        t.d(n, {
          Graph: function () {
            return Ce
          },
          GraphPage: function () {
            return be
          },
          default: function () {
            return ve
          },
        })
      var r = t(809),
        i = t.n(r),
        o = t(92447),
        l = t(85893),
        s = t(59999),
        a = t(26265),
        c = t(64121),
        u = t(67294)
      function d(e, n) {
        var t,
          r = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {},
          i = h(e, null !== (t = r.storage) && void 0 !== t ? t : localStorage),
          o = i.get(),
          l = void 0 !== o ? o : n
        l !== o && i.update(l)
        var s = (0, u.useState)(l),
          a = s[0],
          c = s[1]
        ;(0, u.useEffect)(
          function () {
            a !== l && c(l)
          },
          [e],
        )
        var d = function (e) {
          e instanceof Function
            ? c(function (n) {
                var t = e(n)
                return i.update(t), t
              })
            : (c(e), i.update(e))
        }
        return [a, d]
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
      var g = t(54533),
        f = t(54309),
        x = t(20233),
        p = t(40980),
        j = t(48017),
        m = t(36194),
        v = [],
        b = {}
      for (var C in m.oY)
        for (var y in m.oY[C]) {
          var k = C + y
          'LinearNone' === k && (k = 'Linear'), v.push(k), (b[k] = m.oY[C][y])
        }
      var w = b,
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
          arrows: !1,
          arrowsLength: 1,
          arrowsPos: 0.5,
          arrowsColor: '',
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
          algorithmOptions: v,
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
          citeDashes: !0,
          citeDashLength: 35,
          citeGapLength: 15,
          citeLinkColor: 'gray.600',
          citeNodeColor: 'black',
        },
        N = { follow: 'zoom', localSame: 'add', zoomPadding: 200, zoomSpeed: 2e3 },
        L = { highlight: 'hover', local: 'click', follow: 'double' },
        z = t(38347),
        T = t(93924),
        R = t(83986),
        D = t(48931),
        P = t(67546),
        E = t(93441),
        Z = t(6569),
        H = t(24189),
        B = t(2827),
        A = t(90454),
        F = t(48420),
        M = t(96699),
        V = t(40155),
        X = t(56769),
        W = t(336),
        _ = t(72026),
        U = t(94096),
        Q = t(64115),
        q = t(88134),
        K = t(48235),
        G = t(67273),
        J = t(15267),
        Y = t(46049),
        $ = t(95818),
        ee = t(47647),
        ne = t(86658),
        te = t(29356)
      function re(e, n) {
        var t = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var r = Object.getOwnPropertySymbols(e)
          n &&
            (r = r.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            t.push.apply(t, r)
        }
        return t
      }
      function ie(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? re(Object(t), !0).forEach(function (n) {
                ;(0, a.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : re(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var oe = function (e) {
          var n = e.physics,
            t = e.setPhysics,
            r = e.threeDim,
            i = e.setThreeDim,
            o = e.filter,
            a = e.setFilter,
            d = e.visuals,
            h = e.setVisuals,
            g = e.mouse,
            f = e.setMouse,
            x = e.behavior,
            p = e.setBehavior,
            m = e.tags,
            v = e.tagColors,
            b = e.setTagColors,
            C = (0, u.useState)(!0),
            y = C[0],
            k = C[1],
            w = (0, u.useContext)(te.N),
            H = w.highlightColor,
            B = w.setHighlightColor,
            $ = [
              'red.500',
              'orange.500',
              'yellow.500',
              'green.500',
              'cyan.500',
              'blue.500',
              'pink.500',
              'purple.500',
              'white',
              'gray.100',
              'gray.200',
              'gray.300',
              'gray.400',
              'gray.500',
              'gray.600',
              'gray.700',
              'gray.800',
              'gray.900',
              'black',
            ]
          return (0, l.jsxs)(l.Fragment, {
            children: [
              (0, l.jsx)(A.R, {
                in: !y,
                children: (0, l.jsx)(j.xu, {
                  position: 'absolute',
                  zIndex: 'overlay',
                  marginTop: 10,
                  marginLeft: 10,
                  display: y ? 'none' : 'block',
                  children: (0, l.jsx)(F.h, {
                    'aria-label': 'Settings',
                    icon: (0, l.jsx)(T.e, {}),
                    onClick: function () {
                      return k(!0)
                    },
                  }),
                }),
              }),
              (0, l.jsx)(A.R, {
                in: y,
                children: (0, l.jsxs)(j.xu, {
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
                    (0, l.jsxs)(j.xu, {
                      display: 'flex',
                      justifyContent: 'space-between',
                      alignItems: 'center',
                      paddingRight: 2,
                      paddingTop: 1,
                      children: [
                        (0, l.jsx)(M.u, {
                          label: '2D',
                          children: (0, l.jsx)(V.z, {
                            onClick: function () {
                              return i(!r)
                            },
                            variant: 'ghost',
                            zIndex: 'overlay',
                            children: r ? '3D' : '2D',
                          }),
                        }),
                        (0, l.jsxs)(j.xu, {
                          display: 'flex',
                          alignItems: 'center',
                          children: [
                            (0, l.jsx)(M.u, {
                              label: 'Reset settings to defaults',
                              children: (0, l.jsx)(F.h, {
                                'aria-label': 'Reset Defaults',
                                icon: (0, l.jsx)(R.A, {}),
                                onClick: function () {
                                  h(O), a(I), f(L), t(S), p(N)
                                },
                                variant: 'none',
                                size: 'sm',
                              }),
                            }),
                            (0, l.jsx)(F.h, {
                              size: 'sm',
                              icon: (0, l.jsx)(D.T, {}),
                              'aria-label': 'Close Tweak Panel',
                              variant: 'ghost',
                              onClick: function () {
                                return k(!1)
                              },
                            }),
                          ],
                        }),
                      ],
                    }),
                    (0, l.jsx)(ne.ZP, {
                      autoHeight: !0,
                      autoHeightMax: 600,
                      autoHide: !0,
                      renderThumbVertical: function (e) {
                        var n = e.style,
                          t = (0, z.Z)(e, ['style'])
                        return (0, l.jsx)(
                          j.xu,
                          ie(
                            ie({}, t),
                            {},
                            { style: ie(ie({}, n), {}, { borderRadius: 10 }), bg: H },
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
                                  (0, l.jsx)(W.X, { size: 'sm', children: 'Filter' }),
                                ],
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
                                      (0, l.jsxs)(U.k, {
                                        justifyContent: 'space-between',
                                        children: [
                                          (0, l.jsx)(Q.x, { children: 'Orphans' }),
                                          (0, l.jsx)(q.r, {
                                            onChange: function () {
                                              a(ie(ie({}, o), {}, { orphans: !o.orphans }))
                                            },
                                            isChecked: o.orphans,
                                          }),
                                        ],
                                      }),
                                      (0, l.jsxs)(U.k, {
                                        justifyContent: 'space-between',
                                        children: [
                                          (0, l.jsx)(Q.x, {
                                            children: 'Link nodes with parent file',
                                          }),
                                          (0, l.jsx)(q.r, {
                                            onChange: function () {
                                              a(ie(ie({}, o), {}, { parents: !o.parents }))
                                            },
                                            isChecked: o.parents,
                                          }),
                                        ],
                                      }),
                                    ],
                                  }),
                                  (0, l.jsxs)(X.UQ, {
                                    padding: 0,
                                    allowToggle: !0,
                                    allowMultiple: !0,
                                    paddingLeft: 3,
                                    children: [
                                      (0, l.jsxs)(X.Qd, {
                                        children: [
                                          (0, l.jsxs)(X.KF, {
                                            children: ['Tag filters', (0, l.jsx)(X.XE, {})],
                                          }),
                                          (0, l.jsx)(X.Hk, {
                                            pr: 0,
                                            mr: 0,
                                            children: (0, l.jsx)(ue, {
                                              highlightColor: H,
                                              filter: o,
                                              setFilter: a,
                                              tags: m,
                                            }),
                                          }),
                                        ],
                                      }),
                                      (0, l.jsxs)(X.Qd, {
                                        children: [
                                          (0, l.jsxs)(X.KF, {
                                            children: ['Tag Colors', (0, l.jsx)(X.XE, {})],
                                          }),
                                          (0, l.jsx)(X.Hk, {
                                            pr: 0,
                                            mr: 0,
                                            children: (0, l.jsx)(de, {
                                              tags: m,
                                              colorList: $,
                                              tagColors: v,
                                              setTagColors: b,
                                              highlightColor: H,
                                            }),
                                          }),
                                        ],
                                      }),
                                    ],
                                  }),
                                ],
                              }),
                            ],
                          }),
                          (0, l.jsxs)(X.Qd, {
                            children: [
                              (0, l.jsx)(X.KF, {
                                display: 'flex',
                                justifyContent: 'space-between',
                                children: (0, l.jsxs)(j.xu, {
                                  display: 'flex',
                                  children: [
                                    (0, l.jsx)(X.XE, { marginRight: 2 }),
                                    (0, l.jsx)(W.X, { size: 'sm', children: 'Physics' }),
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
                                      (0, l.jsx)(ae, {
                                        label: 'Gravity',
                                        value: n.gravityOn,
                                        onChange: function () {
                                          return t(ie(ie({}, n), {}, { gravityOn: !n.gravityOn }))
                                        },
                                        children: (0, l.jsx)(se, {
                                          label: 'Strength',
                                          value: 10 * n.gravity,
                                          onChange: function (e) {
                                            return t(ie(ie({}, n), {}, { gravity: e / 10 }))
                                          },
                                        }),
                                      }),
                                      (0, l.jsx)(se, {
                                        value: -n.charge / 100,
                                        onChange: function (e) {
                                          return t(ie(ie({}, n), {}, { charge: -100 * e }))
                                        },
                                        label: 'Repulsive Force',
                                      }),
                                      (0, l.jsx)(ae, {
                                        label: 'Collision',
                                        infoText: 'Perfomance sap, disable if slow',
                                        value: n.collision,
                                        onChange: function () {
                                          return t(ie(ie({}, n), {}, { collision: !n.collision }))
                                        },
                                        children: (0, l.jsx)(se, {
                                          value: n.collisionStrength / 5,
                                          onChange: function (e) {
                                            return t(
                                              ie(ie({}, n), {}, { collisionStrength: 5 * e }),
                                            )
                                          },
                                          label: 'Collision Radius',
                                          infoText:
                                            'Easy with this one, high values can lead to a real jiggly mess',
                                        }),
                                      }),
                                      (0, l.jsx)(se, {
                                        value: 5 * n.linkStrength,
                                        onChange: function (e) {
                                          return t(ie(ie({}, n), {}, { linkStrength: e / 5 }))
                                        },
                                        label: 'Link Force',
                                      }),
                                      (0, l.jsx)(se, {
                                        label: 'Link Iterations',
                                        value: n.linkIts,
                                        onChange: function (e) {
                                          return t(ie(ie({}, n), {}, { linkIts: e }))
                                        },
                                        min: 0,
                                        max: 6,
                                        step: 1,
                                        infoText:
                                          'How many links down the line the physics of a single node affects (Slow)',
                                      }),
                                      (0, l.jsx)(se, {
                                        label: 'Viscosity',
                                        value: 10 * n.velocityDecay,
                                        onChange: function (e) {
                                          return t(ie(ie({}, n), {}, { velocityDecay: e / 10 }))
                                        },
                                      }),
                                    ],
                                  }),
                                  (0, l.jsx)(j.xu, {
                                    children: (0, l.jsx)(X.UQ, {
                                      paddingLeft: 3,
                                      allowToggle: !0,
                                      children: (0, l.jsxs)(X.Qd, {
                                        children: [
                                          (0, l.jsxs)(X.KF, {
                                            children: [
                                              (0, l.jsx)(Q.x, { children: 'Advanced' }),
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
                                                (0, l.jsx)(se, {
                                                  label: 'Stabilization rate',
                                                  value: 50 * n.alphaDecay,
                                                  onChange: function (e) {
                                                    return t(
                                                      ie(ie({}, n), {}, { alphaDecay: e / 50 }),
                                                    )
                                                  },
                                                }),
                                                (0, l.jsx)(ae, {
                                                  label: 'Center nodes',
                                                  value: n.centering,
                                                  onChange: function () {
                                                    return t(
                                                      ie(
                                                        ie({}, n),
                                                        {},
                                                        { centering: !n.centering },
                                                      ),
                                                    )
                                                  },
                                                  infoText:
                                                    'Keeps the nodes in the center of the viewport. If disabled you can drag the nodes anywhere you want.',
                                                  children: (0, l.jsx)(se, {
                                                    label: 'Centering Strength',
                                                    value: n.centeringStrength,
                                                    max: 2,
                                                    step: 0.01,
                                                    onChange: function (e) {
                                                      return t(
                                                        ie(ie({}, n), {}, { centeringStrength: e }),
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
                                  (0, l.jsx)(W.X, { size: 'sm', children: 'Visual' }),
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
                                            children: (0, l.jsxs)(U.k, {
                                              justifyContent: 'space-between',
                                              w: '100%',
                                              children: [
                                                (0, l.jsx)(Q.x, { children: 'Colors' }),
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
                                              children: (0, l.jsxs)(j.xu, {
                                                children: [
                                                  (0, l.jsxs)(U.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(Q.x, { children: 'Nodes' }),
                                                      (0, l.jsx)(M.u, {
                                                        label: 'Shuffle node colors',
                                                        children: (0, l.jsx)(F.h, {
                                                          'aria-label': 'Shuffle node colors',
                                                          size: 'sm',
                                                          icon: (0, l.jsx)(P.n, {}),
                                                          variant: 'ghost',
                                                          onClick: function () {
                                                            var e,
                                                              n =
                                                                null !== (e = d.nodeColorScheme) &&
                                                                void 0 !== e
                                                                  ? e
                                                                  : []
                                                            h(
                                                              ie(
                                                                ie({}, d),
                                                                {},
                                                                {
                                                                  nodeColorScheme: n
                                                                    .map(function (e) {
                                                                      return [Math.random(), e]
                                                                    })
                                                                    .sort(function (e, n) {
                                                                      return (
                                                                        (0, c.Z)(e, 1)[0] -
                                                                        (0, c.Z)(n, 1)[0]
                                                                      )
                                                                    })
                                                                    .map(function (e) {
                                                                      var n = (0, c.Z)(e, 2)
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
                                                        children: (0, l.jsx)(F.h, {
                                                          'aria-label': 'Shift node colors',
                                                          icon: (0, l.jsx)(E.L, {}),
                                                          size: 'sm',
                                                          variant: 'ghost',
                                                          onClick: function () {
                                                            var e,
                                                              n =
                                                                null !== (e = d.nodeColorScheme) &&
                                                                void 0 !== e
                                                                  ? e
                                                                  : []
                                                            h(
                                                              ie(
                                                                ie({}, d),
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
                                                      (0, l.jsxs)(K.v2, {
                                                        placement: 'right',
                                                        closeOnSelect: !1,
                                                        matchWidth: !0,
                                                        children: [
                                                          (0, l.jsx)(K.j2, {
                                                            width: 20,
                                                            as: V.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(Z.v, {}),
                                                            children: (0, l.jsx)(U.k, {
                                                              height: 6,
                                                              width: 6,
                                                              flexDirection: 'column',
                                                              flexWrap: 'wrap',
                                                              children: d.nodeColorScheme.map(
                                                                function (e) {
                                                                  return (0, l.jsx)(
                                                                    j.xu,
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
                                                          (0, l.jsxs)(G.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(K.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: (0, l.jsx)(K.__, {
                                                                  width: 500,
                                                                  type: 'checkbox',
                                                                  defaultValue: d.nodeColorScheme,
                                                                  onChange: function (e) {
                                                                    e.length &&
                                                                      h(
                                                                        ie(
                                                                          ie({}, d),
                                                                          {},
                                                                          { nodeColorScheme: e },
                                                                        ),
                                                                      )
                                                                  },
                                                                  children: $.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      K.ii,
                                                                      {
                                                                        isChecked:
                                                                          d.nodeColorScheme.some(
                                                                            function (n) {
                                                                              return n === e
                                                                            },
                                                                          ),
                                                                        value: e,
                                                                        isDisabled:
                                                                          1 ===
                                                                            d.nodeColorScheme
                                                                              .length &&
                                                                          d.nodeColorScheme[0] ===
                                                                            e,
                                                                        children: (0, l.jsx)(j.xu, {
                                                                          justifyContent:
                                                                            'space-between',
                                                                          alignItems: 'center',
                                                                          display: 'flex',
                                                                          children: (0, l.jsx)(
                                                                            j.xu,
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
                                                  (0, l.jsxs)(U.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(Q.x, { children: 'Links' }),
                                                      (0, l.jsxs)(K.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(K.j2, {
                                                            as: V.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(Z.v, {}),
                                                            children: (0, l.jsx)(j.xu, {
                                                              children: d.linkColorScheme
                                                                ? (0, l.jsx)(j.xu, {
                                                                    bgColor: d.linkColorScheme,
                                                                    borderRadius: 'sm',
                                                                    height: 6,
                                                                    width: 6,
                                                                  })
                                                                : (0, l.jsx)(U.k, {
                                                                    height: 6,
                                                                    width: 6,
                                                                    flexDirection: 'column',
                                                                    flexWrap: 'wrap',
                                                                    children: d.nodeColorScheme.map(
                                                                      function (e) {
                                                                        return (0, l.jsx)(
                                                                          j.xu,
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
                                                          (0, l.jsxs)(G.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsxs)(K.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: [
                                                                  (0, l.jsx)(K.sN, {
                                                                    onClick: function () {
                                                                      return h(
                                                                        ie(
                                                                          ie({}, d),
                                                                          {},
                                                                          { linkColorScheme: '' },
                                                                        ),
                                                                      )
                                                                    },
                                                                    justifyContent: 'space-between',
                                                                    alignItems: 'center',
                                                                    display: 'flex',
                                                                    children: (0, l.jsx)(U.k, {
                                                                      height: 6,
                                                                      width: 6,
                                                                      flexDirection: 'column',
                                                                      flexWrap: 'wrap',
                                                                      children:
                                                                        d.nodeColorScheme.map(
                                                                          function (e) {
                                                                            return (0, l.jsx)(
                                                                              j.xu,
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
                                                                  $.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      K.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            ie(
                                                                              ie({}, d),
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
                                                                        children: (0, l.jsx)(j.xu, {
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
                                                  (0, l.jsxs)(U.k, {
                                                    alignItems: 'center',
                                                    justifyContent: 'space-between',
                                                    children: [
                                                      (0, l.jsx)(Q.x, { children: 'Accent' }),
                                                      (0, l.jsxs)(K.v2, {
                                                        placement: 'right',
                                                        children: [
                                                          (0, l.jsx)(K.j2, {
                                                            as: V.z,
                                                            colorScheme: '',
                                                            color: 'black',
                                                            rightIcon: (0, l.jsx)(Z.v, {}),
                                                            children: (0, l.jsx)(j.xu, {
                                                              bgColor: H,
                                                              borderRadius: 'sm',
                                                              height: 6,
                                                              width: 6,
                                                            }),
                                                          }),
                                                          (0, l.jsxs)(G.h, {
                                                            children: [
                                                              ' ',
                                                              (0, l.jsx)(K.qy, {
                                                                minW: 10,
                                                                zIndex: 'popover',
                                                                bgColor: 'gray.200',
                                                                children: $.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    K.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return B(e)
                                                                      },
                                                                      justifyContent:
                                                                        'space-between',
                                                                      alignItems: 'center',
                                                                      display: 'flex',
                                                                      children: (0, l.jsx)(j.xu, {
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
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Link highlight',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'linkHighlight',
                                                    visValue: d.linkHighlight,
                                                  }),
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Node highlight',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'nodeHighlight',
                                                    visValue: d.nodeHighlight,
                                                  }),
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Background',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'backgroundColor',
                                                    visValue: d.backgroundColor,
                                                  }),
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Emacs node',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'emacsNodeColor',
                                                    visValue: d.emacsNodeColor,
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
                                        (0, l.jsx)(se, {
                                          label: 'Node size',
                                          value: d.nodeRel,
                                          onChange: function (e) {
                                            return h(ie(ie({}, d), {}, { nodeRel: e }))
                                          },
                                        }),
                                        r &&
                                          (0, l.jsxs)(l.Fragment, {
                                            children: [
                                              (0, l.jsx)(se, {
                                                label: 'Node opacity',
                                                value: d.nodeOpacity,
                                                min: 0,
                                                max: 1,
                                                onChange: function (e) {
                                                  return h(ie(ie({}, d), {}, { nodeOpacity: e }))
                                                },
                                              }),
                                              (0, l.jsx)(se, {
                                                label: 'Node resolution',
                                                value: d.nodeResolution,
                                                min: 5,
                                                max: 32,
                                                step: 1,
                                                onChange: function (e) {
                                                  return h(ie(ie({}, d), {}, { nodeResolution: e }))
                                                },
                                              }),
                                            ],
                                          }),
                                        (0, l.jsx)(se, {
                                          label: 'Link width',
                                          value: d.linkWidth,
                                          onChange: function (e) {
                                            return h(ie(ie({}, d), {}, { linkWidth: e }))
                                          },
                                        }),
                                        r &&
                                          (0, l.jsx)(se, {
                                            label: 'Link opacity',
                                            min: 0,
                                            max: 1,
                                            value: d.linkOpacity,
                                            onChange: function (e) {
                                              return h(ie(ie({}, d), {}, { linkOpacity: e }))
                                            },
                                          }),
                                        (0, l.jsxs)(ae, {
                                          label: 'Dash cite links',
                                          infoText:
                                            'Add dashes to citation links made with org-roam-bibtex',
                                          value: d.citeDashes,
                                          onChange: function () {
                                            return h(
                                              ie(ie({}, d), {}, { citeDashes: !d.citeDashes }),
                                            )
                                          },
                                          children: [
                                            (0, l.jsx)(se, {
                                              label: 'Dash length',
                                              value: d.citeDashLength / 10,
                                              onChange: function (e) {
                                                return h(
                                                  ie(ie({}, d), {}, { citeDashLength: 10 * e }),
                                                )
                                              },
                                            }),
                                            (0, l.jsx)(se, {
                                              label: 'Gap length',
                                              value: d.citeGapLength / 5,
                                              onChange: function (e) {
                                                return h(
                                                  ie(ie({}, d), {}, { citeGapLength: 5 * e }),
                                                )
                                              },
                                            }),
                                          ],
                                        }),
                                        (0, l.jsx)(ce, {
                                          colorList: $,
                                          label: 'Citation node color',
                                          visuals: d,
                                          setVisuals: h,
                                          value: 'citeNodeColor',
                                          visValue: d.citeNodeColor,
                                        }),
                                        (0, l.jsx)(ce, {
                                          colorList: $,
                                          label: 'Citationlink color',
                                          visuals: d,
                                          setVisuals: h,
                                          value: 'citeLinkColor',
                                          visValue: d.citeLinkColor,
                                        }),
                                        (0, l.jsxs)(j.xu, {
                                          children: [
                                            (0, l.jsxs)(U.k, {
                                              alignItems: 'center',
                                              justifyContent: 'space-between',
                                              children: [
                                                (0, l.jsx)(Q.x, { children: 'Labels' }),
                                                (0, l.jsxs)(K.v2, {
                                                  placement: 'right',
                                                  children: [
                                                    (0, l.jsx)(K.j2, {
                                                      as: V.z,
                                                      colorScheme: '',
                                                      color: 'black',
                                                      rightIcon: (0, l.jsx)(Z.v, {}),
                                                      children: d.labels
                                                        ? d.labels < 2
                                                          ? 'On Highlight'
                                                          : 'Always'
                                                        : 'Never',
                                                    }),
                                                    (0, l.jsxs)(G.h, {
                                                      children: [
                                                        ' ',
                                                        (0, l.jsxs)(K.qy, {
                                                          zIndex: 'popover',
                                                          bgColor: 'gray.200',
                                                          children: [
                                                            (0, l.jsx)(K.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  ie(ie({}, d), {}, { labels: 0 }),
                                                                )
                                                              },
                                                              children: 'Never',
                                                            }),
                                                            (0, l.jsx)(K.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  ie(ie({}, d), {}, { labels: 1 }),
                                                                )
                                                              },
                                                              children: 'On Highlight',
                                                            }),
                                                            (0, l.jsx)(K.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  ie(ie({}, d), {}, { labels: 2 }),
                                                                )
                                                              },
                                                              children: 'Always',
                                                            }),
                                                            (0, l.jsx)(K.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  ie(ie({}, d), {}, { labels: 3 }),
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
                                              in: d.labels > 0,
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
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Text',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'labelTextColor',
                                                    visValue: d.labelTextColor,
                                                  }),
                                                  (0, l.jsx)(ce, {
                                                    colorList: $,
                                                    label: 'Text',
                                                    visuals: d,
                                                    setVisuals: h,
                                                    value: 'labelBackgroundColor',
                                                    visValue: d.labelBackgroundColor,
                                                  }),
                                                  (0, l.jsx)(J.U, {
                                                    in: !!d.labelBackgroundColor,
                                                    animateOpacity: !0,
                                                    children: (0, l.jsx)(j.xu, {
                                                      paddingTop: 2,
                                                      children: (0, l.jsx)(se, {
                                                        label: 'Background opacity',
                                                        value: d.labelBackgroundOpacity,
                                                        onChange: function (e) {
                                                          console.log(d.labelBackgroundOpacity),
                                                            h(
                                                              ie(
                                                                ie({}, d),
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
                                                    in: d.labels > 1,
                                                    animateOpacity: !0,
                                                    children: (0, l.jsx)(j.xu, {
                                                      paddingTop: 2,
                                                      children: (0, l.jsx)(se, {
                                                        label: 'Label Appearance Scale',
                                                        value: 5 * d.labelScale,
                                                        onChange: function (e) {
                                                          return h(
                                                            ie(
                                                              ie({}, d),
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
                                        (0, l.jsxs)(ae, {
                                          label: 'Link arrows',
                                          value: d.arrows,
                                          onChange: function () {
                                            return h(ie(ie({}, d), {}, { arrows: !d.arrows }))
                                          },
                                          children: [
                                            (0, l.jsx)(se, {
                                              label: 'Arrow size',
                                              value: d.arrowsLength / 10,
                                              onChange: function (e) {
                                                return h(
                                                  ie(ie({}, d), {}, { arrowsLength: 10 * e }),
                                                )
                                              },
                                            }),
                                            (0, l.jsx)(se, {
                                              label: 'Arrow Position',
                                              value: d.arrowsPos,
                                              min: 0,
                                              max: 1,
                                              step: 0.01,
                                              onChange: function (e) {
                                                return h(ie(ie({}, d), {}, { arrowsPos: e }))
                                              },
                                            }),
                                            (0, l.jsx)(ce, {
                                              colorList: $,
                                              label: 'Arrow Color',
                                              visuals: d,
                                              setVisuals: h,
                                              value: 'arrowsColor',
                                              visValue: d.arrowsColor,
                                            }),
                                          ],
                                        }),
                                        (0, l.jsxs)(ae, {
                                          label: 'Directional Particles',
                                          value: d.particles,
                                          onChange: function () {
                                            return h(ie(ie({}, d), {}, { particles: !d.particles }))
                                          },
                                          children: [
                                            (0, l.jsx)(se, {
                                              label: 'Particle Number',
                                              value: d.particlesNumber,
                                              max: 5,
                                              step: 1,
                                              onChange: function (e) {
                                                return h(ie(ie({}, d), {}, { particlesNumber: e }))
                                              },
                                            }),
                                            (0, l.jsx)(se, {
                                              label: 'Particle Size',
                                              value: d.particlesWidth,
                                              onChange: function (e) {
                                                return h(ie(ie({}, d), {}, { particlesWidth: e }))
                                              },
                                            }),
                                          ],
                                        }),
                                        (0, l.jsx)(ae, {
                                          label: 'Highlight',
                                          onChange: function () {
                                            return h(ie(ie({}, d), {}, { highlight: !d.highlight }))
                                          },
                                          value: d.highlight,
                                          children: (0, l.jsxs)(_.gC, {
                                            spacing: 1,
                                            justifyContent: 'flex-start',
                                            divider: (0, l.jsx)(_.cX, { borderColor: 'gray.400' }),
                                            align: 'stretch',
                                            paddingLeft: 0,
                                            children: [
                                              (0, l.jsx)(se, {
                                                label: 'Highlight Link Thickness',
                                                value: d.highlightLinkSize,
                                                onChange: function (e) {
                                                  return h(
                                                    ie(ie({}, d), {}, { highlightLinkSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsx)(se, {
                                                label: 'Highlight Node Size',
                                                value: d.highlightNodeSize,
                                                onChange: function (e) {
                                                  return h(
                                                    ie(ie({}, d), {}, { highlightNodeSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsxs)(ae, {
                                                label: 'Highlight Animation',
                                                onChange: function () {
                                                  h(
                                                    ie(
                                                      ie({}, d),
                                                      {},
                                                      { highlightAnim: !d.highlightAnim },
                                                    ),
                                                  )
                                                },
                                                value: d.highlightAnim,
                                                children: [
                                                  (0, l.jsx)(se, {
                                                    label: 'Animation speed',
                                                    onChange: function (e) {
                                                      return h(
                                                        ie(ie({}, d), {}, { animationSpeed: e }),
                                                      )
                                                    },
                                                    value: d.animationSpeed,
                                                    infoText:
                                                      'Slower speed has a chance of being buggy',
                                                    min: 50,
                                                    max: 1e3,
                                                    step: 10,
                                                  }),
                                                  (0, l.jsx)(Y.Ph, {
                                                    placeholder: d.algorithmName,
                                                    onChange: function (e) {
                                                      h(
                                                        ie(
                                                          ie({}, d),
                                                          {},
                                                          { algorithmName: e.target.value },
                                                        ),
                                                      )
                                                    },
                                                    children: d.algorithmOptions.map(function (e) {
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
                                  (0, l.jsx)(W.X, { size: 'sm', children: 'Behavior' }),
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
                                    (0, l.jsxs)(U.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsxs)(U.k, {
                                          children: [
                                            (0, l.jsx)(Q.x, { children: 'Expand Node' }),
                                            (0, l.jsx)(le, {
                                              infoText:
                                                'View only the node and its direct neighbors',
                                            }),
                                          ],
                                        }),
                                        (0, l.jsxs)(K.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(K.j2, {
                                              as: V.z,
                                              rightIcon: (0, l.jsx)(Z.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(Q.x, {
                                                children: g.local
                                                  ? g.local[0].toUpperCase() + g.local.slice(1)
                                                  : 'Never',
                                              }),
                                            }),
                                            (0, l.jsxs)(G.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(K.qy, {
                                                  zIndex: 'popover',
                                                  bgColor: 'gray.200',
                                                  children: [
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(ie(ie({}, g), {}, { local: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { local: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { local: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { local: 'right' }),
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
                                    (0, l.jsxs)(U.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(Q.x, { children: 'Open in Emacs' }),
                                        (0, l.jsxs)(K.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(K.j2, {
                                              as: V.z,
                                              rightIcon: (0, l.jsx)(Z.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(Q.x, {
                                                children: g.follow
                                                  ? g.follow[0].toUpperCase() + g.follow.slice(1)
                                                  : 'Never',
                                              }),
                                            }),
                                            (0, l.jsxs)(G.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(K.qy, {
                                                  bgColor: 'gray.200',
                                                  zIndex: 'popover',
                                                  children: [
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(ie(ie({}, g), {}, { follow: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { follow: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { follow: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return f(
                                                          ie(ie({}, g), {}, { follow: 'right' }),
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
                                    (0, l.jsxs)(U.k, {
                                      alignItems: 'center',
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, l.jsx)(Q.x, { children: 'Follow Emacs by...' }),
                                        (0, l.jsxs)(K.v2, {
                                          placement: 'right',
                                          children: [
                                            (0, l.jsx)(K.j2, {
                                              as: V.z,
                                              rightIcon: (0, l.jsx)(Z.v, {}),
                                              colorScheme: '',
                                              color: 'black',
                                              children: (0, l.jsx)(Q.x, {
                                                children:
                                                  x.follow[0].toUpperCase() + x.follow.slice(1),
                                              }),
                                            }),
                                            (0, l.jsxs)(G.h, {
                                              children: [
                                                ' ',
                                                (0, l.jsxs)(K.qy, {
                                                  bgColor: 'gray.200',
                                                  zIndex: 'popover',
                                                  children: [
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return p(
                                                          ie(ie({}, x), {}, { follow: 'local' }),
                                                        )
                                                      },
                                                      children: 'Opening the local graph',
                                                    }),
                                                    (0, l.jsx)(K.sN, {
                                                      onClick: function () {
                                                        return p(
                                                          ie(ie({}, x), {}, { follow: 'zoom' }),
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
                                    (0, l.jsx)(se, {
                                      label: 'Zoom speed',
                                      value: x.zoomSpeed,
                                      min: 0,
                                      max: 4e3,
                                      step: 100,
                                      onChange: function (e) {
                                        return p(ie(ie({}, x), {}, { zoomSpeed: e }))
                                      },
                                    }),
                                    (0, l.jsx)(se, {
                                      label: 'Zoom padding',
                                      value: x.zoomPadding,
                                      min: 0,
                                      max: 400,
                                      step: 1,
                                      onChange: function (e) {
                                        return p(ie(ie({}, x), {}, { zoomPadding: e }))
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
        le = function (e) {
          var n = e.infoText
          return (0, l.jsx)(j.xu, {
            paddingLeft: '1',
            children: (0, l.jsx)(M.u, {
              label: n,
              placement: 'top',
              color: 'gray.100',
              bg: 'gray.800',
              hasArrow: !0,
              children: (0, l.jsx)(H.h, {}),
            }),
          })
        },
        se = function (e) {
          var n = e.min,
            t = void 0 === n ? 0 : n,
            r = e.max,
            i = void 0 === r ? 10 : r,
            o = e.step,
            s = void 0 === o ? 0.1 : o,
            a = e.value,
            c = void 0 === a ? 1 : a,
            d = (0, z.Z)(e, ['min', 'max', 'step', 'value']),
            h = d.onChange,
            g = d.label,
            f = d.infoText,
            x = (0, u.useContext)(te.N).highlightColor
          return (0, l.jsxs)(j.xu, {
            children: [
              (0, l.jsxs)(j.xu, {
                display: 'flex',
                alignItems: 'flex-end',
                children: [(0, l.jsx)(Q.x, { children: g }), f && (0, l.jsx)(le, { infoText: f })],
              }),
              (0, l.jsxs)($.iR, {
                value: c,
                onChange: h,
                min: t,
                max: i,
                step: s,
                children: [
                  (0, l.jsx)($.Uj, { children: (0, l.jsx)($.Ms, {}) }),
                  (0, l.jsx)(M.u, {
                    bg: x,
                    label: c.toFixed(1),
                    children: (0, l.jsx)($.gs, { bg: 'white' }),
                  }),
                ],
              }),
            ],
          })
        },
        ae = function (e) {
          var n = e.value,
            t = e.onChange,
            r = e.label,
            i = e.infoText,
            o = e.children
          return (0, l.jsxs)(j.xu, {
            paddingTop: 2,
            children: [
              (0, l.jsxs)(j.xu, {
                display: 'flex',
                justifyContent: 'space-between',
                paddingBottom: 2,
                children: [
                  (0, l.jsxs)(j.xu, {
                    display: 'flex',
                    alignItems: 'center',
                    children: [
                      (0, l.jsx)(Q.x, { children: r }),
                      i && (0, l.jsx)(le, { infoText: i }),
                    ],
                  }),
                  (0, l.jsx)(q.r, { isChecked: !!n, onChange: t }),
                ],
              }),
              (0, l.jsx)(J.U, {
                in: !!n,
                animateOpacity: !0,
                children: (0, l.jsx)(j.xu, {
                  paddingLeft: 4,
                  paddingTop: 2,
                  paddingBottom: 2,
                  children: o,
                }),
              }),
            ],
          })
        },
        ce = function (e) {
          var n = e.label,
            t = e.colorList,
            r = e.value,
            i = e.visuals,
            o = e.visValue,
            s = e.setVisuals
          return (0, l.jsxs)(U.k, {
            alignItems: 'center',
            justifyContent: 'space-between',
            children: [
              (0, l.jsx)(Q.x, { children: n }),
              (0, l.jsxs)(K.v2, {
                placement: 'right',
                children: [
                  (0, l.jsx)(K.j2, {
                    as: V.z,
                    colorScheme: '',
                    color: 'black',
                    rightIcon: (0, l.jsx)(Z.v, {}),
                    children: (0, l.jsx)(j.xu, {
                      bgColor: o,
                      borderRadius: 'sm',
                      height: 6,
                      width: 6,
                    }),
                  }),
                  (0, l.jsxs)(G.h, {
                    children: [
                      ' ',
                      (0, l.jsxs)(K.qy, {
                        minW: 10,
                        zIndex: 'popover',
                        bgColor: 'gray.200',
                        children: [
                          (0, l.jsx)(K.sN, {
                            onClick: function () {
                              return s(ie(ie({}, i), {}, (0, a.Z)({}, r, '')))
                            },
                            justifyContent: 'space-between',
                            alignItems: 'center',
                            display: 'flex',
                            children: (0, l.jsx)(j.xu, { height: 6, width: 6 }),
                          }),
                          t.map(function (e) {
                            return (0, l.jsx)(
                              K.sN,
                              {
                                onClick: function () {
                                  return s(ie(ie({}, i), {}, (0, a.Z)({}, r, e)))
                                },
                                justifyContent: 'space-between',
                                alignItems: 'center',
                                display: 'flex',
                                children: (0, l.jsx)(j.xu, {
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
          })
        },
        ue = function (e) {
          var n = e.filter,
            t = e.setFilter,
            r = e.tags,
            i = e.highlightColor,
            o = r.map(function (e) {
              return { value: e, label: e }
            }),
            s = (0, u.useState)(
              n.tags.map(function (e) {
                return { value: e, label: e }
              }),
            ),
            a = s[0],
            c = s[1]
          return (0, l.jsx)(ee.CUIAutoComplete, {
            items: o,
            label: 'Add tag to filter',
            placeholder: ' ',
            onCreateItem: function (e) {
              return null
            },
            disableCreateItem: !0,
            selectedItems: a,
            onSelectedItemsChange: function (e) {
              e.selectedItems &&
                (c(e.selectedItems),
                t(
                  ie(
                    ie({}, n),
                    {},
                    {
                      tags: e.selectedItems.map(function (e) {
                        return e.value
                      }),
                    },
                  ),
                ))
            },
            listItemStyleProps: { overflow: 'hidden' },
            highlightItemBg: 'gray.400',
            toggleButtonStyleProps: { variant: 'outline' },
            inputStyleProps: { focusBorderColor: i, color: 'gray.800', borderColor: 'gray.600' },
            tagStyleProps: {
              rounded: 'full',
              bg: i,
              height: 8,
              paddingLeft: 4,
              fontWeight: 'bold',
            },
            hideToggleButton: !0,
            itemRenderer: function (e) {
              return e.label
            },
          })
        },
        de = function (e) {
          var n = e.colorList,
            t = e.tagColors,
            r = e.setTagColors,
            i = e.highlightColor,
            o = e.tags.map(function (e) {
              return { value: e, label: e }
            }),
            s = (0, u.useState)(
              Object.keys(t).map(function (e) {
                return { value: e, label: e }
              }),
            ),
            c = s[0],
            d = s[1]
          return (0, l.jsxs)(j.xu, {
            children: [
              (0, l.jsx)(ee.CUIAutoComplete, {
                items: o,
                label: 'Add tag to filter',
                placeholder: ' ',
                disableCreateItem: !0,
                selectedItems: c,
                onSelectedItemsChange: function (e) {
                  e.selectedItems &&
                    (d(Array.from(new Set(e.selectedItems))),
                    r(
                      Object.fromEntries(
                        Array.from(new Set(e.selectedItems)).map(function (e) {
                          var n
                          return [
                            e.label,
                            null !== (n = t[e.label]) && void 0 !== n ? n : 'gray.600',
                          ]
                        }),
                      ),
                    ))
                },
                listItemStyleProps: { overflow: 'hidden' },
                highlightItemBg: 'gray.400',
                toggleButtonStyleProps: { variant: 'outline' },
                inputStyleProps: {
                  focusBorderColor: i,
                  color: 'gray.800',
                  borderColor: 'gray.600',
                },
                tagStyleProps: {
                  display: 'none',
                  rounded: 'full',
                  bg: i,
                  height: 8,
                  paddingLeft: 4,
                  fontWeight: 'bold',
                },
                hideToggleButton: !0,
                itemRenderer: function (e) {
                  return e.label
                },
              }),
              (0, l.jsx)(_.gC, {
                spacing: 2,
                justifyContent: 'flex-start',
                divider: (0, l.jsx)(_.cX, { borderColor: 'gray.500' }),
                align: 'stretch',
                color: 'gray.800',
                children: Object.keys(t).map(function (e) {
                  return (0, l.jsxs)(
                    U.k,
                    {
                      alignItems: 'center',
                      justifyContent: 'space-between',
                      width: '100%',
                      pl: 2,
                      children: [
                        (0, l.jsx)(j.xu, {
                          width: '100%',
                          children: (0, l.jsx)(Q.x, { fontWeight: 'bold', children: e }),
                        }),
                        (0, l.jsxs)(K.v2, {
                          isLazy: !0,
                          placement: 'right',
                          children: [
                            (0, l.jsx)(K.j2, {
                              as: V.z,
                              colorScheme: '',
                              color: 'black',
                              children: (0, l.jsx)(j.xu, {
                                bgColor: t[e],
                                borderRadius: 'sm',
                                height: 6,
                                width: 6,
                              }),
                            }),
                            (0, l.jsxs)(G.h, {
                              children: [
                                ' ',
                                (0, l.jsx)(K.qy, {
                                  minW: 10,
                                  zIndex: 'popover',
                                  bgColor: 'gray.200',
                                  children: n.map(function (n) {
                                    return (0, l.jsx)(
                                      K.sN,
                                      {
                                        onClick: function () {
                                          return r(ie(ie({}, t), {}, (0, a.Z)({}, e, n)))
                                        },
                                        justifyContent: 'space-between',
                                        alignItems: 'center',
                                        display: 'flex',
                                        children: (0, l.jsx)(j.xu, {
                                          bgColor: n,
                                          borderRadius: 'sm',
                                          height: 6,
                                          width: 6,
                                        }),
                                      },
                                      n,
                                    )
                                  }),
                                }),
                              ],
                            }),
                          ],
                        }),
                        (0, l.jsx)(F.h, {
                          'aria-label': 'Delete tag color',
                          variant: 'ghost',
                          icon: (0, l.jsx)(B.p, {}),
                          onClick: function () {
                            r(
                              Object.fromEntries(
                                Array.from(new Set(c)).map(function (e) {
                                  var n
                                  return [
                                    e.label,
                                    null !== (n = t[e.label]) && void 0 !== n ? n : 'gray.600',
                                  ]
                                }),
                              ),
                            ),
                              d(
                                c.filter(function (n) {
                                  return n.value !== e
                                }),
                              )
                          },
                        }),
                      ],
                    },
                    e,
                  )
                }),
              }),
            ],
          })
        },
        he = t(31122),
        ge = t(22003)
      function fe(e, n) {
        var t = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var r = Object.getOwnPropertySymbols(e)
          n &&
            (r = r.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            t.push.apply(t, r)
        }
        return t
      }
      function xe(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? fe(Object(t), !0).forEach(function (n) {
                ;(0, a.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : fe(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var pe = t.e(4).then(t.bind(t, 57004)),
        je = t.g.window ? t(61957).f$ : null,
        me = t.g.window ? t(61957).s6 : null
      function ve() {
        var e = (0, u.useState)(!1),
          n = e[0],
          t = e[1]
        return (
          (0, u.useEffect)(function () {
            t(!0)
          }, []),
          n ? (0, l.jsx)(be, {}) : null
        )
      }
      function be() {
        var e = d('physics', S),
          n = (0, c.Z)(e, 2),
          t = n[0],
          r = n[1],
          i = d('filter', I),
          o = (0, c.Z)(i, 2),
          h = o[0],
          g = o[1],
          f = d('visuals', O),
          x = (0, c.Z)(f, 2),
          p = x[0],
          m = x[1],
          v = (0, u.useState)(null),
          b = v[0],
          C = v[1],
          y = (0, u.useState)(null),
          k = y[0],
          w = y[1],
          z = d('behavior', N),
          T = (0, c.Z)(z, 2),
          R = T[0],
          D = T[1],
          P = d('mouse', L),
          E = (0, c.Z)(P, 2),
          Z = E[0],
          H = E[1],
          B = (0, u.useRef)({}),
          A = (0, u.useRef)({}),
          F = (0, u.useRef)([]),
          M = (0, u.useContext)(te.N).setEmacsTheme,
          V = d('3d', !1),
          X = (0, c.Z)(V, 2),
          W = X[0],
          _ = X[1],
          U = d('tagCols', {}),
          Q = (0, c.Z)(U, 2),
          q = Q[0],
          K = Q[1],
          G = (0, u.useState)({ nodeIds: [] }),
          J = G[0],
          Y = G[1],
          $ = (0, u.useRef)({ nodeIds: [] }),
          ee = (0, u.useRef)(N)
        ee.current = R
        var ne = (0, u.useRef)(null),
          re = (0, u.useRef)(null)
        $.current = J
        var ie = function (e, n) {
          var t,
            r = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : 2e3,
            i = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : 200,
            o = ne.current,
            l = $.current,
            a = ee.current,
            c = null !== (t = A.current[n]) && void 0 !== t ? t : [],
            u = Object.fromEntries(
              [n]
                .concat(
                  (0, s.Z)(
                    c.flatMap(function (e) {
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
                (console.log('emptying'), console.log('scope ' + l.nodeIds), Y({ nodeIds: [] })),
              void setTimeout(function () {
                return o.zoomToFit(r, i, function (e) {
                  return u[e.id]
                })
              }, 50))
            : l.nodeIds.length
            ? 'add' !== a.localSame
              ? (Y({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.centerAt(0, 0, r)
                }, 50))
              : l.nodeIds.includes(n) &&
                l.nodeIds.some(function (e) {
                  return u[e]
                })
              ? (Y(function (e) {
                  return xe(xe({}, e), {}, { nodeIds: [].concat((0, s.Z)(e.nodeIds), [n]) })
                }),
                void setTimeout(function () {
                  return o.zoomToFit(r, i, function (e) {
                    return u[e.id]
                  })
                }, 50))
              : (Y({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.centerAt(0, 0, r)
                }, 50))
            : (Y({ nodeIds: [n] }),
              void setTimeout(function () {
                o.centerAt(0, 0, r)
              }, 50))
        }
        return (
          (0, u.useEffect)(function () {
            ;(re.current = new ge.Z('ws://localhost:35903')),
              re.current.addEventListener('open', function (e) {
                console.log('Connection with Emacs established')
              }),
              re.current.addEventListener('message', function (e) {
                ne.current
                var n = ee.current,
                  t = JSON.parse(e.data)
                switch (t.type) {
                  case 'graphdata':
                    return (function (e) {
                      var n
                      F.current = null !== (n = e.tags) && void 0 !== n ? n : []
                      var t = e.nodes.reduce(function (e, n) {
                          var t
                          return xe(
                            xe({}, e),
                            {},
                            (0, a.Z)(
                              {},
                              n.file,
                              [].concat(
                                (0, s.Z)(null !== (t = e[n.file]) && void 0 !== t ? t : []),
                                [n],
                              ),
                            ),
                          )
                        }, {}),
                        r = Object.keys(t).flatMap(function (e) {
                          var n,
                            r = null !== (n = t[e]) && void 0 !== n ? n : [],
                            i = r.find(function (e) {
                              return 0 === e.level
                            }),
                            o = r.filter(function (e) {
                              return 0 !== e.level
                            })
                          return i
                            ? o.map(function (e) {
                                return { source: e.id, target: i.id, type: 'parent' }
                              })
                            : []
                        })
                      B.current = Object.fromEntries(
                        e.nodes.map(function (e) {
                          return [e.id, e]
                        }),
                      )
                      var i = [].concat((0, s.Z)(e.links), (0, s.Z)(r)).filter(function (e) {
                        var n = e.source,
                          t = e.target
                        return B.current[n] && B.current[t]
                      })
                      A.current = i.reduce(function (e, n) {
                        var t, r, i
                        return xe(
                          xe({}, e),
                          {},
                          ((i = {}),
                          (0, a.Z)(
                            i,
                            n.source,
                            [].concat(
                              (0, s.Z)(null !== (t = e[n.source]) && void 0 !== t ? t : []),
                              [n],
                            ),
                          ),
                          (0, a.Z)(
                            i,
                            n.target,
                            [].concat(
                              (0, s.Z)(null !== (r = e[n.target]) && void 0 !== r ? r : []),
                              [n],
                            ),
                          ),
                          i),
                        )
                      }, {})
                      var o = xe(xe({}, e), {}, { links: i }),
                        l = JSON.parse(JSON.stringify(o))
                      C(l)
                    })(t.data)
                  case 'theme':
                    return M(t.data)
                  case 'command':
                    switch (t.data.commandName) {
                      case 'local':
                        var r = R.zoomSpeed,
                          i = R.zoomPadding
                        ie('local', t.data.id, r, i), w(t.data.id)
                        break
                      case 'zoom':
                        var o,
                          l,
                          c =
                            (null === t || void 0 === t || null === (o = t.data) || void 0 === o
                              ? void 0
                              : o.speed) || n.zoomSpeed,
                          u =
                            (null === t || void 0 === t || null === (l = t.data) || void 0 === l
                              ? void 0
                              : l.padding) || n.zoomPadding
                        ie('zoom', t.data.id, c, u), w(t.data.id)
                        break
                      case 'follow':
                        ie(n.follow, t.data.id, n.zoomSpeed, n.zoomPadding), w(t.data.id)
                        break
                      default:
                        return console.error('unknown message type', t.type)
                    }
                }
              })
          }, []),
          b
            ? (0, l.jsxs)(j.xu, {
                display: 'flex',
                alignItems: 'flex-start',
                flexDirection: 'row',
                height: '100%',
                children: [
                  (0, l.jsx)(
                    oe,
                    xe(
                      xe(
                        {},
                        {
                          physics: t,
                          setPhysics: r,
                          threeDim: W,
                          setThreeDim: _,
                          filter: h,
                          setFilter: g,
                          visuals: p,
                          setVisuals: m,
                          mouse: Z,
                          setMouse: H,
                          behavior: R,
                          setBehavior: D,
                          tagColors: q,
                          setTagColors: K,
                        },
                      ),
                      {},
                      { tags: F.current },
                    ),
                  ),
                  (0, l.jsx)(j.xu, {
                    position: 'absolute',
                    alignItems: 'top',
                    children: (0, l.jsx)(
                      Ce,
                      xe(
                        {
                          ref: ne,
                          nodeById: B.current,
                          linksByNodeId: A.current,
                          webSocket: re.current,
                        },
                        {
                          physics: t,
                          graphData: b,
                          threeDim: W,
                          emacsNodeId: k,
                          filter: h,
                          visuals: p,
                          behavior: R,
                          mouse: Z,
                          scope: J,
                          setScope: Y,
                          tagColors: q,
                        },
                      ),
                    ),
                  }),
                ],
              })
            : null
        )
      }
      var Ce = (0, u.forwardRef)(function (e, n) {
        var t = e.physics,
          r = e.graphData,
          a = e.threeDim,
          d = e.linksByNodeId,
          h = e.filter,
          j = e.emacsNodeId,
          m = e.nodeById,
          v = e.visuals,
          b = (e.behavior, e.mouse),
          C = e.scope,
          y = e.setScope,
          k = e.webSocket,
          S = e.tagColors,
          I = (0, f.iP)(),
          O = (0, c.Z)(I, 2),
          N = O[0],
          L = O[1],
          z = (0, u.useState)(null),
          T = z[0],
          R = z[1],
          D = (0, p.useTheme)(),
          P = (0, u.useContext)(te.N).emacsTheme,
          E = function (e, n) {
            switch (e) {
              case b.local:
                if (C.nodeIds.includes(n.id)) break
                y(function (e) {
                  return xe(xe({}, e), {}, { nodeIds: [].concat((0, s.Z)(e.nodeIds), [n.id]) })
                })
                break
              case b.follow:
                k.send(n.id)
            }
          },
          Z = (0, u.useRef)(null)
        ;(0, u.useEffect)(
          function () {
            j && R(m[j])
          },
          [j],
        ),
          (Z.current = T)
        var H = (0, u.useMemo)(
            function () {
              if (!Z.current) return {}
              var e = d[Z.current.id]
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
            [Z.current, d],
          ),
          B = (0, u.useMemo)(
            function () {
              var e = r.nodes.filter(function (e) {
                  var n,
                    t = e,
                    r = null !== (n = d[t.id]) && void 0 !== n ? n : []
                  return h.tags.length && t.tags.length
                    ? !h.tags.some(function (e) {
                        return t.tags.indexOf(e) > -1
                      })
                    : !h.orphans ||
                        (h.parents
                          ? 0 !== r.length
                          : 0 !== r.length &&
                            r.some(function (e) {
                              return !['parent', 'ref'].includes(e.type)
                            }))
                }),
                n = e.map(function (e) {
                  return e.id
                })
              return {
                filteredNodes: e,
                filteredLinks: r.links.filter(function (e) {
                  if (h.tags.length) {
                    var t = 'object' === typeof e.source ? e.source.id : e.source,
                      r = 'object' === typeof e.target ? e.target.id : e.target
                    return n.includes(t) && n.includes(r)
                  }
                  var i = e
                  return h.parents || 'parent' !== i.type
                }),
              }
            },
            [h, r],
          ),
          A = (0, u.useMemo)(
            function () {
              var e = B.filteredNodes.filter(function (e) {
                  var n,
                    t = null !== (n = d[e.id]) && void 0 !== n ? n : []
                  return (
                    C.nodeIds.includes(e.id) ||
                    t.some(function (e) {
                      return C.nodeIds.includes(e.source) || C.nodeIds.includes(e.target)
                    })
                  )
                }),
                n = e.map(function (e) {
                  return e.id
                }),
                t = B.filteredLinks.filter(function (e) {
                  var t = 'object' === typeof e.source ? e.source.id : e.source,
                    r = 'object' === typeof e.target ? e.target.id : e.target
                  return n.includes(t) && n.includes(r)
                })
              return 0 === C.nodeIds.length
                ? { nodes: B.filteredNodes, links: B.filteredLinks }
                : { nodes: e, links: t }
            },
            [h, C, r],
          )
        ;(0, u.useEffect)(function () {
          ;(0, o.Z)(
            i().mark(function e() {
              var r, o
              return i().wrap(function (e) {
                for (;;)
                  switch ((e.prev = e.next)) {
                    case 0:
                      return (r = n.current), (e.next = 3), pe
                    case 3:
                      ;(o = e.sent),
                        t.gravityOn
                          ? (r.d3Force('x', o.forceX().strength(t.gravity)),
                            r.d3Force('y', o.forceY().strength(t.gravity)),
                            a && r.d3Force('z', o.forceZ().strength(t.gravity)))
                          : (r.d3Force('x', null), r.d3Force('y', null), a && r.d3Force('z', null)),
                        t.centering
                          ? r.d3Force('center', o.forceCenter().strength(t.centeringStrength))
                          : r.d3Force('center', null),
                        t.linkStrength && r.d3Force('link').strength(t.linkStrength),
                        t.linkIts && r.d3Force('link').iterations(t.linkIts),
                        t.charge && r.d3Force('charge').strength(t.charge),
                        r.d3Force(
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
          (0, u.useEffect)(
            function () {
              var e
              null === (e = n.current) || void 0 === e || e.d3ReheatSimulation()
            },
            [t],
          )
        var F = (0, u.useRef)(0),
          M = (0, u.useState)(1),
          V = M[0],
          X = M[1],
          W = (0, x._7)(
            function (e) {
              return X(e)
            },
            { duration: v.animationSpeed, algorithm: w[v.algorithmName] },
          ),
          _ = (0, c.Z)(W, 2),
          U = _[0],
          Q = _[1],
          q = (0, x._7)(
            function (e) {
              return X(Math.min(V, -1 * (e - 1)))
            },
            { duration: v.animationSpeed, algorithm: w[v.algorithmName] },
          ),
          K = (0, c.Z)(q, 2),
          G = K[0],
          J = K[1],
          Y = (0, u.useRef)(null)
        ;(0, u.useEffect)(
          function () {
            if ((T && (Y.current = T), !v.highlightAnim)) return X(T ? 1 : 0)
            T ? U() : (Q(), V > 0.5 ? G() : X(0))
          },
          [T],
        )
        var $ = function (e) {
            if (D)
              return e.split('.').reduce(function (e, n) {
                return e[n]
              }, D.colors)
          },
          ee = (0, u.useMemo)(
            function () {
              var e = v.nodeColorScheme.concat(
                v.linkColorScheme || [],
                v.linkHighlight || [],
                v.nodeHighlight || [],
                v.citeNodeColor || [],
                v.citeLinkColor || [],
              )
              return Object.fromEntries(
                e.map(function (n) {
                  var t = $(n),
                    r = e.map(function (e) {
                      return [e, g.Z(t, $(e))]
                    })
                  return [n, Object.fromEntries(r)]
                }),
              )
            },
            [v.nodeColorScheme, v.linkHighlight, v.nodeHighlight, v.linkColorScheme, P],
          ),
          ne = (0, u.useMemo)(
            function () {
              var e,
                n,
                t,
                r =
                  null !== (e = d[null === (n = Y.current) || void 0 === n ? void 0 : n.id]) &&
                  void 0 !== e
                    ? e
                    : []
              return Object.fromEntries(
                [null === (t = Y.current) || void 0 === t ? void 0 : t.id]
                  .concat(
                    (0, s.Z)(
                      r.flatMap(function (e) {
                        return [e.source, e.target]
                      }),
                    ),
                  )
                  .map(function (e) {
                    return [e, {}]
                  }),
              )
            },
            [JSON.stringify(T), Y.current],
          ),
          re = function (e) {
            var n,
              t,
              r,
              i,
              o,
              l,
              s =
                null !== (n = null === (t = d[e]) || void 0 === t ? void 0 : t.length) &&
                void 0 !== n
                  ? n
                  : 0,
              a = s
                ? null === (r = d[e]) || void 0 === r
                  ? void 0
                  : r.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                : 0,
              c = h.parents ? s : s - a
            return v.nodeColorScheme[
              ((i = c), (o = 0), (l = v.nodeColorScheme.length - 1), Math.min(Math.max(i, o), l))
            ]
          },
          ie = function (e, n) {
            return d[e] > d[n] ? re(e) : re(n)
          },
          oe = function (e, n) {
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
          le = (0, u.useMemo)(
            function () {
              return $(v.labelTextColor)
            },
            [v.labelTextColor, P],
          ),
          se = (0, u.useMemo)(
            function () {
              return $(v.labelBackgroundColor)
            },
            [v.labelBackgroundColor, P],
          ),
          ae = {
            graphData: A,
            width: N,
            height: L,
            backgroundColor: D.colors.gray[v.backgroundColor],
            nodeLabel: function (e) {
              return e.title
            },
            nodeColor: function (e) {
              return (function (e) {
                var n = H[e.id] || ne[e.id]
                if (v.emacsNodeColor && e.id === j) return $(v.emacsNodeColor)
                if (
                  S &&
                  e.tags.some(function (e) {
                    return S[e]
                  })
                ) {
                  var t =
                    S[
                      e.tags.filter(function (e) {
                        return S[e]
                      })[0]
                    ]
                  return $(t)
                }
                return v.citeNodeColor && e.properties.ROAM_REFS
                  ? $(v.citeNodeColor)
                  : n && v.nodeHighlight
                  ? ee[re(e.id)][v.nodeHighlight](V)
                  : $(re(e.id))
              })(e)
            },
            nodeRelSize: v.nodeRel,
            nodeVal: function (e) {
              var n,
                t = null !== (n = d[e.id]) && void 0 !== n ? n : [],
                r = t.length
                  ? t.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                  : 0
              return (
                (3 + t.length - (h.parents ? 0 : r)) *
                (H[e.id] || ne[e.id] ? 1 + V * (v.highlightNodeSize - 1) : 1)
              )
            },
            nodeCanvasObject: function (e, n, t) {
              if (e && v.labels) {
                var r = ne[e.id]
                if (!(t <= v.labelScale || 1 === v.labels) || H[e.id] || r) {
                  var i = e.title,
                    o = i.substring(0, Math.min(i.length, 40)),
                    l = 12 / t,
                    a = [1.1 * n.measureText(o).width, l].map(function (e) {
                      return e + 0.5 * l
                    }),
                    c = Math.min((3 * (t - v.labelScale)) / v.labelScale, 1),
                    u = function () {
                      return 1 === v.labels || t <= v.labelScale
                        ? V
                        : H[e.id] || ne[e.id]
                        ? Math.max(c, V)
                        : 1 * c * (-1 * (0.5 * V - 1))
                    }
                  if (v.labelBackgroundColor && v.labelBackgroundOpacity) {
                    var d = u() * v.labelBackgroundOpacity,
                      h = oe(se, d)
                    ;(n.fillStyle = h),
                      n.fillRect.apply(n, [e.x - a[0] / 2, e.y - a[1] / 2].concat((0, s.Z)(a)))
                  }
                  var g = u()
                  ;(n.textAlign = 'center'), (n.textBaseline = 'middle')
                  var f = oe(le, g)
                  ;(n.fillStyle = f),
                    (n.font = ''.concat(l, 'px Sans-Serif')),
                    n.fillText(o, e.x, e.y)
                }
              }
            },
            nodeCanvasObjectMode: function () {
              return 'after'
            },
            linkDirectionalParticles: v.particles ? v.particlesNumber : void 0,
            linkDirectionalArrowLength: v.arrows ? v.arrowsLength : void 0,
            linkDirectionalArrowRelPos: v.arrowsPos,
            linkDirectionalArrowColor: v.arrowsColor
              ? function (e) {
                  return $(v.arrowsColor)
                }
              : void 0,
            linkColor: function (e) {
              var n = 'object' === typeof e.source ? e.source.id : e.source,
                t = 'object' === typeof e.target ? e.target.id : e.target,
                r = ye(e, Z.current),
                i = ye(e, Y.current),
                o = r || i,
                l = e
              return v.citeLinkColor && 'cite' === l.type
                ? $(v.citeLinkColor)
                : (function (e, n, t) {
                    if (!v.linkHighlight && !v.linkColorScheme && !t) {
                      var r = ie(e, n)
                      return $(r)
                    }
                    if (!t && !v.linkColorScheme) {
                      var i = ie(e, n)
                      return $(i)
                    }
                    if (!t) return $(v.linkColorScheme)
                    if (!v.linkHighlight && !v.linkColorScheme) {
                      var o = ie(e, n)
                      return $(o)
                    }
                    return v.linkHighlight
                      ? v.linkColorScheme
                        ? ee[v.linkColorScheme][v.linkHighlight](V)
                        : ee[ie(e, n)][v.linkHighlight](V)
                      : $(v.linkColorScheme)
                  })(n, t, o)
            },
            linkWidth: function (e) {
              var n = ye(e, Z.current),
                t = ye(e, Y.current)
              return n || t ? v.linkWidth * (1 + V * (v.highlightLinkSize - 1)) : v.linkWidth
            },
            linkDirectionalParticleWidth: v.particlesWidth,
            d3AlphaDecay: t.alphaDecay,
            d3AlphaMin: t.alphaMin,
            d3VelocityDecay: t.velocityDecay,
            onNodeClick: function (e, n) {
              var t = n.timeStamp - F.current < 400
              return (F.current = n.timeStamp), E(t ? 'double' : 'click', e)
            },
            onBackgroundClick: function () {
              R(null),
                0 !== C.nodeIds.length &&
                  y(function (e) {
                    return xe(xe({}, e), {}, { nodeIds: [] })
                  })
            },
            onNodeHover: function (e) {
              v.highlight && (T || (J(), X(0)), R(e))
            },
            onNodeRightClick: function (e) {
              E('right', e)
            },
          }
        return (0, l.jsx)('div', {
          children: a
            ? (0, l.jsx)(
                me,
                xe(
                  xe({ ref: n }, ae),
                  {},
                  {
                    nodeThreeObjectExtend: !0,
                    backgroundColor: D.colors.white,
                    nodeOpacity: v.nodeOpacity,
                    nodeResolution: v.nodeResolution,
                    linkOpacity: v.linkOpacity,
                    nodeThreeObject: function (e) {
                      if (v.labels && (!(v.labels < 3) || H[e.id])) {
                        var n = new he.Z(e.title.substring(0, 40))
                        return (
                          (n.color = $(v.labelTextColor)),
                          (n.backgroundColor = $(v.labelBackgroundColor)),
                          (n.padding = 2),
                          (n.textHeight = 8),
                          n
                        )
                      }
                    },
                  },
                ),
              )
            : (0, l.jsx)(
                je,
                xe(
                  xe({ ref: n }, ae),
                  {},
                  {
                    linkLineDash: function (e) {
                      var n = e
                      return v.citeDashes && 'cite' === n.type
                        ? [v.citeDashLength, v.citeGapLength]
                        : null
                    },
                  },
                ),
              ),
        })
      })
      function ye(e, n) {
        return (
          e.source.id === (null === n || void 0 === n ? void 0 : n.id) ||
          e.target.id === (null === n || void 0 === n ? void 0 : n.id)
        )
      }
    },
    45301: function (e, n, t) {
      ;(window.__NEXT_P = window.__NEXT_P || []).push([
        '/',
        function () {
          return t(374)
        },
      ])
    },
  },
  function (e) {
    e.O(0, [774, 737, 446, 13, 888, 179], function () {
      return (n = 45301), e((e.s = n))
      var n
    })
    var n = e.O()
    _N_E = n
  },
])
