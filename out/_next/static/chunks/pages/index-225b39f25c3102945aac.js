;(self.webpackChunk_N_E = self.webpackChunk_N_E || []).push([
  [405],
  {
    374: function (e, n, r) {
      'use strict'
      r.r(n),
        r.d(n, {
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
      var t = r(7757),
        i = r.n(t),
        o = r(2137),
        l = r(5893),
        s = r(7329),
        c = r(6156),
        a = r(4699),
        d = r(7294)
      function u(e, n) {
        var r,
          t = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {},
          i = h(e, null !== (r = t.storage) && void 0 !== r ? r : localStorage),
          o = i.get(),
          l = void 0 !== o ? o : n
        l !== o && i.update(l)
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
                var r = e(n)
                return i.update(r), r
              })
            : (a(e), i.update(e))
        }
        return [c, u]
      }
      function h(e, n) {
        return {
          get: function () {
            var r = n.getItem(e)
            if (r && 'undefined' !== r) return JSON.parse(r)
          },
          update: function (r) {
            n.setItem(e, JSON.stringify(r))
          },
          remove: function () {
            n.removeItem(e)
          },
        }
      }
      var g = r(4533),
        x = r(4309),
        f = r(2351),
        j = r(980),
        p = r(8017),
        m = r(6194),
        b = [],
        C = {}
      for (var v in m.oY)
        for (var y in m.oY[v]) {
          var k = v + y
          'LinearNone' === k && (k = 'Linear'), b.push(k), (C[k] = m.oY[v][y])
        }
      var w = C,
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
        N = {
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
        O = { follow: 'zoom', localSame: 'add', zoomPadding: 200, zoomSpeed: 2e3 },
        z = { highlight: 'hover', local: 'click', follow: 'double' },
        R = r(7375),
        T = r(3924),
        D = r(3986),
        P = r(9641),
        H = r(7546),
        E = r(3441),
        L = r(6569),
        Z = r(4189),
        F = r(454),
        B = r(8420),
        M = r(6699),
        W = r(155),
        A = r(6769),
        X = r(336),
        _ = r(2026),
        q = r(4096),
        U = r(4115),
        Q = r(8134),
        V = r(8235),
        K = r(7273),
        J = r(5267),
        Y = r(6049),
        G = r(3014),
        $ = r(6658),
        ee = r(9356)
      function ne(e, n) {
        var r = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var t = Object.getOwnPropertySymbols(e)
          n &&
            (t = t.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            r.push.apply(r, t)
        }
        return r
      }
      function re(e) {
        for (var n = 1; n < arguments.length; n++) {
          var r = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? ne(Object(r), !0).forEach(function (n) {
                ;(0, c.Z)(e, n, r[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(r))
            : ne(Object(r)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(r, n))
              })
        }
        return e
      }
      var te = function (e) {
          var n = e.physics,
            r = e.setPhysics,
            t = e.threeDim,
            i = e.setThreeDim,
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
            C = m[1],
            v = (0, d.useContext)(ee.N),
            y = v.highlightColor,
            k = v.setHighlightColor,
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
            Z = [
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
              (0, l.jsx)(F.R, {
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
                      return C(!0)
                    },
                  }),
                }),
              }),
              (0, l.jsx)(F.R, {
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
                              return i(!t)
                            },
                            variant: 'ghost',
                            zIndex: 'overlay',
                            children: t ? '3D' : '2D',
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
                                  h(N), c(I), x(z), r(S), j(O)
                                },
                                variant: 'none',
                                size: 'sm',
                              }),
                            }),
                            (0, l.jsx)(B.h, {
                              size: 'sm',
                              icon: (0, l.jsx)(P.T, {}),
                              'aria-label': 'Close Tweak Panel',
                              variant: 'ghost',
                              onClick: function () {
                                return C(!1)
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
                          r = (0, R.Z)(e, ['style'])
                        return (0, l.jsx)(
                          p.xu,
                          re(
                            re({}, r),
                            {},
                            { style: re(re({}, n), {}, { borderRadius: 10 }), bg: y },
                          ),
                        )
                      },
                      children: (0, l.jsxs)(A.UQ, {
                        allowMultiple: !0,
                        allowToggle: !0,
                        color: 'black',
                        children: [
                          (0, l.jsxs)(A.Qd, {
                            children: [
                              (0, l.jsxs)(A.KF, {
                                children: [
                                  (0, l.jsx)(A.XE, { marginRight: 2 }),
                                  (0, l.jsx)(X.X, { size: 'sm', children: 'Filter' }),
                                ],
                              }),
                              (0, l.jsx)(A.Hk, {
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
                                            c(re(re({}, o), {}, { orphans: !o.orphans }))
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
                                            c(re(re({}, o), {}, { parents: !o.parents }))
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
                          (0, l.jsxs)(A.Qd, {
                            children: [
                              (0, l.jsx)(A.KF, {
                                display: 'flex',
                                justifyContent: 'space-between',
                                children: (0, l.jsxs)(p.xu, {
                                  display: 'flex',
                                  children: [
                                    (0, l.jsx)(A.XE, { marginRight: 2 }),
                                    (0, l.jsx)(X.X, { size: 'sm', children: 'Physics' }),
                                  ],
                                }),
                              }),
                              (0, l.jsxs)(A.Hk, {
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
                                          return r(re(re({}, n), {}, { gravityOn: !n.gravityOn }))
                                        },
                                        children: (0, l.jsx)(oe, {
                                          label: 'Strength',
                                          value: 10 * n.gravity,
                                          onChange: function (e) {
                                            return r(re(re({}, n), {}, { gravity: e / 10 }))
                                          },
                                        }),
                                      }),
                                      (0, l.jsx)(oe, {
                                        value: -n.charge / 100,
                                        onChange: function (e) {
                                          return r(re(re({}, n), {}, { charge: -100 * e }))
                                        },
                                        label: 'Repulsive Force',
                                      }),
                                      (0, l.jsx)(le, {
                                        label: 'Collision',
                                        infoText: 'Perfomance sap, disable if slow',
                                        value: n.collision,
                                        onChange: function () {
                                          return r(re(re({}, n), {}, { collision: !n.collision }))
                                        },
                                        children: (0, l.jsx)(oe, {
                                          value: n.collisionStrength / 5,
                                          onChange: function (e) {
                                            return r(
                                              re(re({}, n), {}, { collisionStrength: 5 * e }),
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
                                          return r(re(re({}, n), {}, { linkStrength: e / 5 }))
                                        },
                                        label: 'Link Force',
                                      }),
                                      (0, l.jsx)(oe, {
                                        label: 'Link Iterations',
                                        value: n.linkIts,
                                        onChange: function (e) {
                                          return r(re(re({}, n), {}, { linkIts: e }))
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
                                          return r(re(re({}, n), {}, { velocityDecay: e / 10 }))
                                        },
                                      }),
                                    ],
                                  }),
                                  (0, l.jsx)(p.xu, {
                                    children: (0, l.jsx)(A.UQ, {
                                      paddingLeft: 3,
                                      allowToggle: !0,
                                      children: (0, l.jsxs)(A.Qd, {
                                        children: [
                                          (0, l.jsxs)(A.KF, {
                                            children: [
                                              (0, l.jsx)(U.x, { children: 'Advanced' }),
                                              (0, l.jsx)(A.XE, { marginRight: 2 }),
                                            ],
                                          }),
                                          (0, l.jsx)(A.Hk, {
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
                                                    return r(
                                                      re(re({}, n), {}, { alphaDecay: e / 50 }),
                                                    )
                                                  },
                                                }),
                                                (0, l.jsx)(le, {
                                                  label: 'Center nodes',
                                                  value: n.centering,
                                                  onChange: function () {
                                                    return r(
                                                      re(
                                                        re({}, n),
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
                                                      return r(
                                                        re(re({}, n), {}, { centeringStrength: e }),
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
                          (0, l.jsxs)(A.Qd, {
                            children: [
                              (0, l.jsxs)(A.KF, {
                                children: [
                                  (0, l.jsx)(A.XE, { marginRight: 2 }),
                                  (0, l.jsx)(X.X, { size: 'sm', children: 'Visual' }),
                                ],
                              }),
                              (0, l.jsx)(A.Hk, {
                                children: (0, l.jsxs)(_.gC, {
                                  justifyContent: 'flex-start',
                                  align: 'stretch',
                                  children: [
                                    (0, l.jsx)(A.UQ, {
                                      allowToggle: !0,
                                      defaultIndex: [0],
                                      paddingLeft: 3,
                                      children: (0, l.jsxs)(A.Qd, {
                                        children: [
                                          (0, l.jsx)(A.KF, {
                                            children: (0, l.jsxs)(q.k, {
                                              justifyContent: 'space-between',
                                              w: '100%',
                                              children: [
                                                (0, l.jsx)(U.x, { children: 'Colors' }),
                                                (0, l.jsx)(A.XE, { marginRight: 2 }),
                                              ],
                                            }),
                                          }),
                                          (0, l.jsx)(A.Hk, {
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
                                                          icon: (0, l.jsx)(H.n, {}),
                                                          variant: 'ghost',
                                                          onClick: function () {
                                                            var e,
                                                              n =
                                                                null !== (e = u.nodeColorScheme) &&
                                                                void 0 !== e
                                                                  ? e
                                                                  : []
                                                            h(
                                                              re(
                                                                re({}, u),
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
                                                          icon: (0, l.jsx)(E.L, {}),
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
                                                              re(
                                                                re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                        re(
                                                                          re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                        re(
                                                                          re({}, u),
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
                                                                  Z.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            re(
                                                                              re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
                                                            children: (0, l.jsx)(p.xu, {
                                                              bgColor: y,
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                          re(
                                                                            re({}, u),
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
                                                                            re(
                                                                              re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                          re(
                                                                            re({}, u),
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
                                                                            re(
                                                                              re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                children: Z.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          re(
                                                                            re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                          re(
                                                                            re({}, u),
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
                                                                            re(
                                                                              re({}, u),
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
                                            return h(re(re({}, u), {}, { nodeRel: e }))
                                          },
                                        }),
                                        t &&
                                          (0, l.jsxs)(l.Fragment, {
                                            children: [
                                              (0, l.jsx)(oe, {
                                                label: 'Node opacity',
                                                value: u.nodeOpacity,
                                                min: 0,
                                                max: 1,
                                                onChange: function (e) {
                                                  return h(re(re({}, u), {}, { nodeOpacity: e }))
                                                },
                                              }),
                                              (0, l.jsx)(oe, {
                                                label: 'Node resolution',
                                                value: u.nodeResolution,
                                                min: 5,
                                                max: 32,
                                                step: 1,
                                                onChange: function (e) {
                                                  return h(re(re({}, u), {}, { nodeResolution: e }))
                                                },
                                              }),
                                            ],
                                          }),
                                        (0, l.jsx)(oe, {
                                          label: 'Link width',
                                          value: u.linkWidth,
                                          onChange: function (e) {
                                            return h(re(re({}, u), {}, { linkWidth: e }))
                                          },
                                        }),
                                        t &&
                                          (0, l.jsx)(oe, {
                                            label: 'Link opacity',
                                            min: 0,
                                            max: 1,
                                            value: u.linkOpacity,
                                            onChange: function (e) {
                                              return h(re(re({}, u), {}, { linkOpacity: e }))
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
                                                      rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                  re(re({}, u), {}, { labels: 0 }),
                                                                )
                                                              },
                                                              children: 'Never',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  re(re({}, u), {}, { labels: 1 }),
                                                                )
                                                              },
                                                              children: 'On Highlight',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  re(re({}, u), {}, { labels: 2 }),
                                                                )
                                                              },
                                                              children: 'Always',
                                                            }),
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  re(re({}, u), {}, { labels: 3 }),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                children: Z.map(function (e) {
                                                                  return (0, l.jsx)(
                                                                    V.sN,
                                                                    {
                                                                      onClick: function () {
                                                                        return h(
                                                                          re(
                                                                            re({}, u),
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
                                                            rightIcon: (0, l.jsx)(L.v, {}),
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
                                                                        re(
                                                                          re({}, u),
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
                                                                  Z.map(function (e) {
                                                                    return (0, l.jsx)(
                                                                      V.sN,
                                                                      {
                                                                        onClick: function () {
                                                                          return h(
                                                                            re(
                                                                              re({}, u),
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
                                                              re(
                                                                re({}, u),
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
                                                            re(
                                                              re({}, u),
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
                                          label: 'Link arrows',
                                          value: u.arrows,
                                          onChange: function () {
                                            return h(re(re({}, u), {}, { arrows: !u.arrows }))
                                          },
                                          children: [
                                            (0, l.jsx)(oe, {
                                              label: 'Arrow size',
                                              value: u.arrowsLength / 10,
                                              onChange: function (e) {
                                                return h(
                                                  re(re({}, u), {}, { arrowsLength: 10 * e }),
                                                )
                                              },
                                            }),
                                            (0, l.jsx)(oe, {
                                              label: 'Arrow Position',
                                              value: u.arrowsPos,
                                              min: 0,
                                              max: 1,
                                              step: 0.01,
                                              onChange: function (e) {
                                                return h(re(re({}, u), {}, { arrowsPos: e }))
                                              },
                                            }),
                                            (0, l.jsxs)(q.k, {
                                              alignItems: 'center',
                                              justifyContent: 'space-between',
                                              children: [
                                                (0, l.jsx)(U.x, { children: 'Arrow Color' }),
                                                (0, l.jsxs)(V.v2, {
                                                  placement: 'right',
                                                  children: [
                                                    (0, l.jsx)(V.j2, {
                                                      as: W.z,
                                                      colorScheme: '',
                                                      color: 'black',
                                                      rightIcon: (0, l.jsx)(L.v, {}),
                                                      children: (0, l.jsx)(p.xu, {
                                                        bgColor: u.arrowsColor,
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
                                                            (0, l.jsx)(V.sN, {
                                                              onClick: function () {
                                                                return h(
                                                                  re(
                                                                    re({}, u),
                                                                    {},
                                                                    { arrowsColor: '' },
                                                                  ),
                                                                )
                                                              },
                                                              justifyContent: 'space-between',
                                                              alignItems: 'center',
                                                              display: 'flex',
                                                              children: (0, l.jsx)(p.xu, {
                                                                height: 6,
                                                                width: 6,
                                                              }),
                                                            }),
                                                            w.map(function (e) {
                                                              return (0, l.jsx)(
                                                                V.sN,
                                                                {
                                                                  onClick: function () {
                                                                    return h(
                                                                      re(
                                                                        re({}, u),
                                                                        {},
                                                                        { arrowsColor: e },
                                                                      ),
                                                                    )
                                                                  },
                                                                  justifyContent: 'space-between',
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
                                                            Z.map(function (e) {
                                                              return (0, l.jsx)(
                                                                V.sN,
                                                                {
                                                                  onClick: function () {
                                                                    return h(
                                                                      re(
                                                                        re({}, u),
                                                                        {},
                                                                        { arrowsColor: e },
                                                                      ),
                                                                    )
                                                                  },
                                                                  justifyContent: 'space-between',
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
                                        (0, l.jsxs)(le, {
                                          label: 'Directional Particles',
                                          value: u.particles,
                                          onChange: function () {
                                            return h(re(re({}, u), {}, { particles: !u.particles }))
                                          },
                                          children: [
                                            (0, l.jsx)(oe, {
                                              label: 'Particle Number',
                                              value: u.particlesNumber,
                                              max: 5,
                                              step: 1,
                                              onChange: function (e) {
                                                return h(re(re({}, u), {}, { particlesNumber: e }))
                                              },
                                            }),
                                            (0, l.jsx)(oe, {
                                              label: 'Particle Size',
                                              value: u.particlesWidth,
                                              onChange: function (e) {
                                                return h(re(re({}, u), {}, { particlesWidth: e }))
                                              },
                                            }),
                                          ],
                                        }),
                                        (0, l.jsx)(le, {
                                          label: 'Highlight',
                                          onChange: function () {
                                            return h(re(re({}, u), {}, { highlight: !u.highlight }))
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
                                                    re(re({}, u), {}, { highlightLinkSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsx)(oe, {
                                                label: 'Highlight Node Size',
                                                value: u.highlightNodeSize,
                                                onChange: function (e) {
                                                  return h(
                                                    re(re({}, u), {}, { highlightNodeSize: e }),
                                                  )
                                                },
                                              }),
                                              (0, l.jsxs)(le, {
                                                label: 'Highlight Animation',
                                                onChange: function () {
                                                  h(
                                                    re(
                                                      re({}, u),
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
                                                        re(re({}, u), {}, { animationSpeed: e }),
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
                                                        re(
                                                          re({}, u),
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
                          (0, l.jsxs)(A.Qd, {
                            children: [
                              (0, l.jsxs)(A.KF, {
                                children: [
                                  (0, l.jsx)(A.XE, { marginRight: 2 }),
                                  (0, l.jsx)(X.X, { size: 'sm', children: 'Behavior' }),
                                ],
                              }),
                              (0, l.jsx)(A.Hk, {
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
                                            (0, l.jsx)(ie, {
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
                                              rightIcon: (0, l.jsx)(L.v, {}),
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
                                                        return x(re(re({}, g), {}, { local: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { local: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { local: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { local: 'right' }),
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
                                              rightIcon: (0, l.jsx)(L.v, {}),
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
                                                        return x(re(re({}, g), {}, { follow: '' }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { follow: 'click' }),
                                                        )
                                                      },
                                                      children: 'Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { follow: 'double' }),
                                                        )
                                                      },
                                                      children: 'Double Click',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return x(
                                                          re(re({}, g), {}, { follow: 'right' }),
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
                                              rightIcon: (0, l.jsx)(L.v, {}),
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
                                                          re(re({}, f), {}, { follow: 'local' }),
                                                        )
                                                      },
                                                      children: 'Opening the local graph',
                                                    }),
                                                    (0, l.jsx)(V.sN, {
                                                      onClick: function () {
                                                        return j(
                                                          re(re({}, f), {}, { follow: 'zoom' }),
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
                                        return j(re(re({}, f), {}, { zoomSpeed: e }))
                                      },
                                    }),
                                    (0, l.jsx)(oe, {
                                      label: 'Zoom padding',
                                      value: f.zoomPadding,
                                      min: 0,
                                      max: 400,
                                      step: 1,
                                      onChange: function (e) {
                                        return j(re(re({}, f), {}, { zoomPadding: e }))
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
        ie = function (e) {
          var n = e.infoText
          return (0, l.jsx)(p.xu, {
            paddingLeft: '1',
            children: (0, l.jsx)(M.u, {
              label: n,
              placement: 'top',
              color: 'gray.100',
              bg: 'gray.800',
              hasArrow: !0,
              children: (0, l.jsx)(Z.h, {}),
            }),
          })
        },
        oe = function (e) {
          var n = e.min,
            r = void 0 === n ? 0 : n,
            t = e.max,
            i = void 0 === t ? 10 : t,
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
                children: [(0, l.jsx)(U.x, { children: g }), x && (0, l.jsx)(ie, { infoText: x })],
              }),
              (0, l.jsxs)(G.iR, {
                value: a,
                onChange: h,
                min: r,
                max: i,
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
            r = e.onChange,
            t = e.label,
            i = e.infoText,
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
                      (0, l.jsx)(U.x, { children: t }),
                      i && (0, l.jsx)(ie, { infoText: i }),
                    ],
                  }),
                  (0, l.jsx)(Q.r, { isChecked: !!n, onChange: r }),
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
        se = r(1122),
        ce = r(2003)
      function ae(e, n) {
        var r = Object.keys(e)
        if (Object.getOwnPropertySymbols) {
          var t = Object.getOwnPropertySymbols(e)
          n &&
            (t = t.filter(function (n) {
              return Object.getOwnPropertyDescriptor(e, n).enumerable
            })),
            r.push.apply(r, t)
        }
        return r
      }
      function de(e) {
        for (var n = 1; n < arguments.length; n++) {
          var r = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? ae(Object(r), !0).forEach(function (n) {
                ;(0, c.Z)(e, n, r[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(r))
            : ae(Object(r)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(r, n))
              })
        }
        return e
      }
      var ue = r.e(4).then(r.bind(r, 7004)),
        he = r.g.window ? r(1957).f$ : null,
        ge = r.g.window ? r(1957).s6 : null
      function xe() {
        var e = (0, d.useState)(!1),
          n = e[0],
          r = e[1]
        return (
          (0, d.useEffect)(function () {
            r(!0)
          }, []),
          n ? (0, l.jsx)(fe, {}) : null
        )
      }
      function fe() {
        var e = u('physics', S),
          n = (0, a.Z)(e, 2),
          r = n[0],
          t = n[1],
          i = u('filter', I),
          o = (0, a.Z)(i, 2),
          h = o[0],
          g = o[1],
          x = u('visuals', N),
          f = (0, a.Z)(x, 2),
          j = f[0],
          m = f[1],
          b = (0, d.useState)(null),
          C = b[0],
          v = b[1],
          y = (0, d.useState)(null),
          k = y[0],
          w = y[1],
          R = u('behavior', O),
          T = (0, a.Z)(R, 2),
          D = T[0],
          P = T[1],
          H = u('mouse', z),
          E = (0, a.Z)(H, 2),
          L = E[0],
          Z = E[1],
          F = (0, d.useRef)({}),
          B = (0, d.useRef)({}),
          M = (0, d.useContext)(ee.N).setEmacsTheme,
          W = (0, d.useState)(!1),
          A = W[0],
          X = W[1],
          _ = (0, d.useState)({ nodeIds: [] }),
          q = _[0],
          U = _[1],
          Q = (0, d.useRef)({ nodeIds: [] }),
          V = (0, d.useRef)(O)
        V.current = D
        var K = (0, d.useRef)(null),
          J = (0, d.useRef)(null)
        Q.current = q
        var Y = function (e, n) {
          var r,
            t = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : 2e3,
            i = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : 200,
            o = K.current,
            l = Q.current,
            c = V.current,
            a = null !== (r = B.current[n]) && void 0 !== r ? r : [],
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
                return o.zoomToFit(t, i, function (e) {
                  return d[e.id]
                })
              }, 50))
            : l.nodeIds.length
            ? 'add' !== c.localSame
              ? (U({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.zoomToFit(t, i, function (e) {
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
                  return o.zoomToFit(t, i, function (e) {
                    return d[e.id]
                  })
                }, 50))
              : (U({ nodeIds: [n] }),
                void setTimeout(function () {
                  o.zoomToFit(t, i, function (e) {
                    return d[e.id]
                  })
                }, 50))
            : (U({ nodeIds: [n] }),
              void setTimeout(function () {
                o.zoomToFit(t, i, function (e) {
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
                  r = JSON.parse(e.data)
                switch (r.type) {
                  case 'graphdata':
                    return (function (e) {
                      var n = e.nodes.reduce(function (e, n) {
                          var r
                          return de(
                            de({}, e),
                            {},
                            (0, c.Z)(
                              {},
                              n.file,
                              [].concat(
                                (0, s.Z)(null !== (r = e[n.file]) && void 0 !== r ? r : []),
                                [n],
                              ),
                            ),
                          )
                        }, {}),
                        r = Object.keys(n).flatMap(function (e) {
                          var r,
                            t = null !== (r = n[e]) && void 0 !== r ? r : [],
                            i = t.find(function (e) {
                              return 0 === e.level
                            }),
                            o = t.filter(function (e) {
                              return 0 !== e.level
                            })
                          return i
                            ? o.map(function (e) {
                                return { source: e.id, target: i.id, type: 'parent' }
                              })
                            : []
                        })
                      F.current = Object.fromEntries(
                        e.nodes.map(function (e) {
                          return [e.id, e]
                        }),
                      )
                      var t = [].concat((0, s.Z)(e.links), (0, s.Z)(r)).filter(function (e) {
                        var n = e.source,
                          r = e.target
                        return F.current[n] && F.current[r]
                      })
                      B.current = t.reduce(function (e, n) {
                        var r, t, i
                        return de(
                          de({}, e),
                          {},
                          ((i = {}),
                          (0, c.Z)(
                            i,
                            n.source,
                            [].concat(
                              (0, s.Z)(null !== (r = e[n.source]) && void 0 !== r ? r : []),
                              [n],
                            ),
                          ),
                          (0, c.Z)(
                            i,
                            n.target,
                            [].concat(
                              (0, s.Z)(null !== (t = e[n.target]) && void 0 !== t ? t : []),
                              [n],
                            ),
                          ),
                          i),
                        )
                      }, {})
                      var i = de(de({}, e), {}, { links: t }),
                        o = JSON.parse(JSON.stringify(i))
                      v(o)
                    })(r.data)
                  case 'theme':
                    return M(r.data)
                  case 'command':
                    switch (r.data.commandName) {
                      case 'local':
                        var t = D.zoomSpeed,
                          i = D.zoomPadding
                        Y('local', r.data.id, t, i), w(r.data.id)
                        break
                      case 'zoom':
                        var o,
                          l,
                          a =
                            (null === r || void 0 === r || null === (o = r.data) || void 0 === o
                              ? void 0
                              : o.speed) || n.zoomSpeed,
                          d =
                            (null === r || void 0 === r || null === (l = r.data) || void 0 === l
                              ? void 0
                              : l.padding) || n.zoomPadding
                        Y('zoom', r.data.id, a, d), w(r.data.id)
                        break
                      case 'follow':
                        Y(n.follow, r.data.id, n.zoomSpeed, n.zoomPadding), w(r.data.id)
                        break
                      default:
                        return console.error('unknown message type', r.type)
                    }
                }
              })
          }, []),
          C
            ? (0, l.jsxs)(p.xu, {
                display: 'flex',
                alignItems: 'flex-start',
                flexDirection: 'row',
                height: '100%',
                children: [
                  (0, l.jsx)(
                    te,
                    de(
                      {},
                      {
                        physics: r,
                        setPhysics: t,
                        threeDim: A,
                        setThreeDim: X,
                        filter: h,
                        setFilter: g,
                        visuals: j,
                        setVisuals: m,
                        mouse: L,
                        setMouse: Z,
                        behavior: D,
                        setBehavior: P,
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
                          nodeById: F.current,
                          linksByNodeId: B.current,
                          webSocket: J.current,
                        },
                        {
                          physics: r,
                          graphData: C,
                          threeDim: A,
                          emacsNodeId: k,
                          filter: h,
                          visuals: j,
                          behavior: D,
                          mouse: L,
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
        var r = e.physics,
          t = e.graphData,
          c = e.threeDim,
          u = e.linksByNodeId,
          h = e.filter,
          p = e.emacsNodeId,
          m = e.nodeById,
          b = e.visuals,
          C = (e.behavior, e.mouse),
          v = e.scope,
          y = e.setScope,
          k = e.webSocket,
          S = (0, x.iP)(),
          I = (0, a.Z)(S, 2),
          N = I[0],
          O = I[1],
          z = (0, d.useState)(null),
          R = z[0],
          T = z[1],
          D = (0, j.useTheme)(),
          P = (0, d.useContext)(ee.N).emacsTheme,
          H = function (e, n) {
            switch (e) {
              case C.local:
                if (v.nodeIds.includes(n.id)) break
                y(function (e) {
                  return de(de({}, e), {}, { nodeIds: [].concat((0, s.Z)(e.nodeIds), [n.id]) })
                })
                break
              case C.follow:
                k.send(n.id)
            }
          },
          E = (0, d.useRef)(null)
        ;(0, d.useEffect)(
          function () {
            p && T(m[p])
          },
          [p],
        ),
          (E.current = R)
        var L = (0, d.useMemo)(
            function () {
              if (!E.current) return {}
              var e = u[E.current.id]
              return e
                ? Object.fromEntries(
                    [E.current.id]
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
            [E.current, u],
          ),
          Z = (0, d.useMemo)(
            function () {
              var e = t.nodes.filter(function (e) {
                  var n,
                    r = null !== (n = u[e.id]) && void 0 !== n ? n : []
                  return (
                    !h.orphans ||
                    (h.parents
                      ? 0 !== r.length
                      : 0 !== r.length &&
                        r.some(function (e) {
                          return !['parent', 'cite', 'ref'].includes(e.type)
                        }))
                  )
                }),
                n =
                  (e.map(function (e) {
                    return e.id
                  }),
                  t.links.filter(function (e) {
                    var n = e
                    return 'cite' !== n.type && (h.parents || 'parent' !== n.type)
                  })),
                r = e.filter(function (e) {
                  var n,
                    r = null !== (n = u[e.id]) && void 0 !== n ? n : []
                  return (
                    v.nodeIds.includes(e.id) ||
                    r.some(function (e) {
                      return v.nodeIds.includes(e.source) || v.nodeIds.includes(e.target)
                    })
                  )
                }),
                i = r.map(function (e) {
                  return e.id
                }),
                o = n.filter(function (e) {
                  var n = 'object' === typeof e.source ? e.source.id : e.source,
                    r = 'object' === typeof e.target ? e.target.id : e.target
                  return i.includes(n) && i.includes(r)
                })
              return 0 === v.nodeIds.length ? { nodes: e, links: n } : { nodes: r, links: o }
            },
            [h, v, t],
          )
        ;(0, d.useEffect)(function () {
          ;(0, o.Z)(
            i().mark(function e() {
              var t, o
              return i().wrap(function (e) {
                for (;;)
                  switch ((e.prev = e.next)) {
                    case 0:
                      return (t = n.current), (e.next = 3), ue
                    case 3:
                      ;(o = e.sent),
                        r.gravityOn
                          ? (t.d3Force('x', o.forceX().strength(r.gravity)),
                            t.d3Force('y', o.forceY().strength(r.gravity)),
                            c && t.d3Force('z', o.forceZ().strength(r.gravity)))
                          : (t.d3Force('x', null), t.d3Force('y', null), c && t.d3Force('z', null)),
                        r.centering
                          ? t.d3Force('center', o.forceCenter().strength(r.centeringStrength))
                          : t.d3Force('center', null),
                        r.linkStrength && t.d3Force('link').strength(r.linkStrength),
                        r.linkIts && t.d3Force('link').iterations(r.linkIts),
                        r.charge && t.d3Force('charge').strength(r.charge),
                        t.d3Force(
                          'collide',
                          r.collision ? o.forceCollide().radius(r.collisionStrength) : null,
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
            [r],
          )
        var F = (0, d.useRef)(0),
          B = (0, d.useState)(1),
          M = B[0],
          W = B[1],
          A = (0, f._7)(
            function (e) {
              return W(e)
            },
            { duration: b.animationSpeed, algorithm: w[b.algorithmName] },
          ),
          X = (0, a.Z)(A, 2),
          _ = X[0],
          q = X[1],
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
                  var r = Y(n),
                    t = e.map(function (e) {
                      return [e, g.Z(r, Y(e))]
                    })
                  return [n, Object.fromEntries(t)]
                }),
              )
            },
            [b.nodeColorScheme, b.linkHighlight, b.nodeHighlight, b.linkColorScheme, P],
          ),
          $ = (0, d.useMemo)(
            function () {
              var e,
                n,
                r,
                t =
                  null !== (e = u[null === (n = J.current) || void 0 === n ? void 0 : n.id]) &&
                  void 0 !== e
                    ? e
                    : []
              return Object.fromEntries(
                [null === (r = J.current) || void 0 === r ? void 0 : r.id]
                  .concat(
                    (0, s.Z)(
                      t.flatMap(function (e) {
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
              r,
              t,
              i,
              o,
              l,
              s =
                null !== (n = null === (r = u[e]) || void 0 === r ? void 0 : r.length) &&
                void 0 !== n
                  ? n
                  : 0,
              c = s
                ? null === (t = u[e]) || void 0 === t
                  ? void 0
                  : t.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                : 0,
              a = h.parents ? s : s - c
            return b.nodeColorScheme[
              ((i = a), (o = 0), (l = b.nodeColorScheme.length - 1), Math.min(Math.max(i, o), l))
            ]
          },
          re = function (e, n) {
            return u[e] > u[n] ? ne(e) : ne(n)
          },
          te = function (e, n) {
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
          ie = (0, d.useMemo)(
            function () {
              return Y(b.labelTextColor)
            },
            [b.labelTextColor, P],
          ),
          oe = (0, d.useMemo)(
            function () {
              return Y(b.labelBackgroundColor)
            },
            [b.labelBackgroundColor, P],
          ),
          le = {
            graphData: Z,
            width: N,
            height: O,
            backgroundColor: D.colors.gray[b.backgroundColor],
            nodeLabel: function (e) {
              return e.title
            },
            nodeColor: function (e) {
              return (function (e) {
                var n = L[e.id] || $[e.id]
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
                r = null !== (n = u[e.id]) && void 0 !== n ? n : [],
                t = r.length
                  ? r.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                  : 0
              return (
                (3 + r.length - (h.parents ? 0 : t)) *
                (L[e.id] || $[e.id] ? 1 + M * (b.highlightNodeSize - 1) : 1)
              )
            },
            nodeCanvasObject: function (e, n, r) {
              if (e && b.labels) {
                var t = $[e.id]
                if (!(r <= b.labelScale || 1 === b.labels) || L[e.id] || t) {
                  var i = e.title,
                    o = i.substring(0, Math.min(i.length, 40)),
                    l = 12 / r,
                    c = [1.1 * n.measureText(o).width, l].map(function (e) {
                      return e + 0.5 * l
                    }),
                    a = Math.min((3 * (r - b.labelScale)) / b.labelScale, 1),
                    d = function () {
                      return 1 === b.labels || r <= b.labelScale
                        ? M
                        : L[e.id] || $[e.id]
                        ? Math.max(a, M)
                        : 1 * a * (-1 * (0.5 * M - 1))
                    }
                  if (b.labelBackgroundColor && b.labelBackgroundOpacity) {
                    var u = d() * b.labelBackgroundOpacity,
                      h = te(oe, u)
                    ;(n.fillStyle = h),
                      n.fillRect.apply(n, [e.x - c[0] / 2, e.y - c[1] / 2].concat((0, s.Z)(c)))
                  }
                  var g = d()
                  ;(n.textAlign = 'center'), (n.textBaseline = 'middle')
                  var x = te(ie, g)
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
            linkDirectionalArrowLength: b.arrows ? b.arrowsLength : void 0,
            linkDirectionalArrowRelPos: b.arrowsPos,
            linkDirectionalArrowColor: b.arrowsColor
              ? function (e) {
                  return Y(b.arrowsColor)
                }
              : void 0,
            linkColor: function (e) {
              var n = 'object' === typeof e.source ? e.source.id : e.source,
                r = 'object' === typeof e.target ? e.target.id : e.target,
                t = pe(e, E.current),
                i = pe(e, J.current)
              return (function (e, n, r) {
                if (!b.linkHighlight && !b.linkColorScheme && !r) {
                  var t = re(e, n)
                  return Y(t)
                }
                if (!r && !b.linkColorScheme) {
                  var i = re(e, n)
                  return Y(i)
                }
                if (!r) return Y(b.linkColorScheme)
                if (!b.linkHighlight && !b.linkColorScheme) {
                  var o = re(e, n)
                  return Y(o)
                }
                return b.linkHighlight
                  ? b.linkColorScheme
                    ? G[b.linkColorScheme][b.linkHighlight](M)
                    : G[re(e, n)][b.linkHighlight](M)
                  : Y(b.linkColorScheme)
              })(n, r, t || i)
            },
            linkWidth: function (e) {
              var n = pe(e, E.current),
                r = pe(e, J.current)
              return n || r ? b.linkWidth * (1 + M * (b.highlightLinkSize - 1)) : b.linkWidth
            },
            linkDirectionalParticleWidth: b.particlesWidth,
            d3AlphaDecay: r.alphaDecay,
            d3AlphaMin: r.alphaMin,
            d3VelocityDecay: r.velocityDecay,
            onNodeClick: function (e, n) {
              var r = n.timeStamp - F.current < 400
              return (F.current = n.timeStamp), H(r ? 'double' : 'click', e)
            },
            onBackgroundClick: function () {
              T(null),
                0 !== v.nodeIds.length &&
                  y(function (e) {
                    return de(de({}, e), {}, { nodeIds: [] })
                  })
            },
            onNodeHover: function (e) {
              b.highlight && (R || (K(), W(0)), T(e))
            },
            onNodeRightClick: function (e) {
              H('right', e)
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
                      if (b.labels && (!(b.labels < 3) || L[e.id])) {
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
    5301: function (e, n, r) {
      ;(window.__NEXT_P = window.__NEXT_P || []).push([
        '/',
        function () {
          return r(374)
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
