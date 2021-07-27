;(self.webpackChunk_N_E = self.webpackChunk_N_E || []).push([
  [405],
  {
    374: function (e, n, t) {
      'use strict'
      t.r(n),
        t.d(n, {
          Graph: function () {
            return le
          },
          GraphPage: function () {
            return ie
          },
          default: function () {
            return re
          },
        })
      var r = t(7757),
        i = t.n(r),
        l = t(2137),
        o = t(5893),
        a = t(7329),
        s = t(6156),
        c = t(4699),
        u = t(7294)
      function d(e, n) {
        var t,
          r = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {},
          i = h(e, null !== (t = r.storage) && void 0 !== t ? t : localStorage),
          l = i.get(),
          o = void 0 !== l ? l : n
        o !== l && i.update(o)
        var a = (0, u.useState)(o),
          s = a[0],
          c = a[1]
        ;(0, u.useEffect)(
          function () {
            s !== o && c(o)
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
        return [s, d]
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
      var f = t(4533),
        g = t(4309),
        p = t(2351),
        x = t(980),
        v = t(8017),
        j = t(6194),
        m = [],
        y = {}
      for (var b in j.oY)
        for (var k in j.oY[b]) {
          var C = b + k
          'LinearNone' === C && (C = 'Linear'), m.push(C), (y[C] = j.oY[b][k])
        }
      var S = {
          enabled: !0,
          charge: -350,
          collision: !0,
          collisionStrength: 0,
          linkStrength: 0.1,
          linkIts: 1,
          particles: !1,
          particlesNumber: 0,
          particlesWidth: 4,
          linkOpacity: 0.4,
          linkWidth: 1,
          nodeRel: 4,
          labels: 2,
          labelScale: 1.5,
          alphaDecay: 0.02,
          alphaTarget: 0,
          alphaMin: 0,
          velocityDecay: 0.25,
          gravity: 0.5,
          gravityOn: !0,
          colorful: !0,
          galaxy: !0,
          ticks: 1,
          hover: 'highlight',
          click: 'select',
          doubleClick: 'local',
          iterations: 0,
          highlight: !0,
          highlightNodeSize: 2,
          highlightLinkSize: 2,
          highlightAnim: !1,
          animationSpeed: 250,
          algorithms: y,
          algorithmOptions: m,
          algorithmName: 'CubicOut',
          orphans: !1,
          follow: 'Local',
        },
        O = { orphans: !1, parents: !0, tags: [], nodes: [], links: [], date: [] },
        w = t(7375),
        N = t(3924),
        I = t(3986),
        D = t(9641),
        P = t(6569),
        T = t(4189),
        z = t(8420),
        E = t(8841),
        F = t(5684),
        L = t(155),
        M = t(6769),
        Z = t(336),
        R = t(2026),
        X = t(4096),
        H = t(4115),
        A = t(8134),
        _ = t(8235),
        W = t(6049),
        B = t(3014),
        J = t(6658)
      function Q(e, n) {
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
      function U(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? Q(Object(t), !0).forEach(function (n) {
                ;(0, s.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : Q(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var K = function (e) {
          var n = e.physics,
            t = e.setPhysics,
            r = e.threeDim,
            i = e.setThreeDim,
            l = e.filter,
            a = e.setFilter,
            s = (0, u.useState)(!0),
            c = s[0],
            d = s[1]
          return (0, o.jsxs)(o.Fragment, {
            children: [
              (0, o.jsx)(v.xu, {
                position: 'relative',
                zIndex: 'overlay',
                marginTop: 10,
                marginLeft: 10,
                display: c ? 'none' : 'block',
                children: (0, o.jsx)(z.h, {
                  'aria-label': 'Settings',
                  icon: (0, o.jsx)(N.e, {}),
                  onClick: function () {
                    return d(!0)
                  },
                }),
              }),
              (0, o.jsx)(E.U, {
                in: c,
                animateOpacity: !0,
                children: (0, o.jsxs)(v.xu, {
                  bg: 'alt.100',
                  w: 'xs',
                  marginTop: 10,
                  marginLeft: 10,
                  borderRadius: 'xl',
                  maxH: 650,
                  paddingBottom: 5,
                  zIndex: 'overlay',
                  position: 'relative',
                  boxShadow: 'xl',
                  children: [
                    (0, o.jsxs)(v.xu, {
                      display: 'flex',
                      justifyContent: 'space-between',
                      alignItems: 'center',
                      children: [
                        (0, o.jsx)(F.u, {
                          label: '2D',
                          children: (0, o.jsx)(L.z, {
                            onClick: function () {
                              return i(!r)
                            },
                            colorScheme: 'purple',
                            variant: r ? 'solid' : 'outline',
                            zIndex: 'overlay',
                            children: r ? '2D' : '3D',
                          }),
                        }),
                        (0, o.jsxs)(v.xu, {
                          display: 'flex',
                          alignItems: 'center',
                          children: [
                            (0, o.jsx)(F.u, {
                              label: 'Reset settings to defaults',
                              children: (0, o.jsx)(z.h, {
                                'aria-label': 'Reset Defaults',
                                icon: (0, o.jsx)(I.A, {}),
                                onClick: function () {
                                  return t(S)
                                },
                                colorScheme: 'purple',
                                variant: 'none',
                                size: 'sm',
                              }),
                            }),
                            (0, o.jsx)(z.h, {
                              size: 'sm',
                              colorScheme: 'purple',
                              icon: (0, o.jsx)(D.T, {}),
                              'aria-label': 'Close Tweak Panel',
                              variant: 'ghost',
                              onClick: function () {
                                return d(!1)
                              },
                            }),
                          ],
                        }),
                      ],
                    }),
                    (0, o.jsx)(J.ZP, {
                      autoHeight: !0,
                      autoHeightMax: 600,
                      autoHide: !0,
                      renderThumbVertical: function (e) {
                        var n = e.style,
                          t = (0, w.Z)(e, ['style'])
                        return (0, o.jsx)(
                          v.xu,
                          U(
                            U({}, t),
                            {},
                            { style: U(U({}, n), {}, { borderRadius: 10 }), bg: 'purple.500' },
                          ),
                        )
                      },
                      children: (0, o.jsxs)(M.UQ, {
                        allowMultiple: !0,
                        allowToggle: !0,
                        color: 'black',
                        paddingRight: 2,
                        children: [
                          (0, o.jsxs)(M.Qd, {
                            children: [
                              (0, o.jsxs)(M.KF, {
                                children: [
                                  (0, o.jsx)(M.XE, { marginRight: 2 }),
                                  (0, o.jsx)(Z.X, { size: 'sm', children: 'Filter' }),
                                ],
                              }),
                              (0, o.jsx)(M.Hk, {
                                children: (0, o.jsxs)(R.gC, {
                                  spacing: 2,
                                  justifyContent: 'flex-start',
                                  divider: (0, o.jsx)(R.cX, { borderColor: 'gray.500' }),
                                  align: 'stretch',
                                  paddingLeft: 7,
                                  color: 'gray.800',
                                  children: [
                                    (0, o.jsxs)(X.k, {
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, o.jsx)(H.x, { children: 'Orphans' }),
                                        (0, o.jsx)(A.r, {
                                          colorScheme: 'purple',
                                          onChange: function () {
                                            a(U(U({}, l), {}, { orphans: !l.orphans }))
                                          },
                                          isChecked: l.orphans,
                                        }),
                                      ],
                                    }),
                                    (0, o.jsxs)(X.k, {
                                      justifyContent: 'space-between',
                                      children: [
                                        (0, o.jsx)(H.x, {
                                          children: 'Link nodes with parent file',
                                        }),
                                        (0, o.jsx)(A.r, {
                                          colorScheme: 'purple',
                                          onChange: function () {
                                            a(U(U({}, l), {}, { parents: !l.parents }))
                                          },
                                          isChecked: l.parents,
                                        }),
                                      ],
                                    }),
                                  ],
                                }),
                              }),
                            ],
                          }),
                          (0, o.jsxs)(M.Qd, {
                            children: [
                              (0, o.jsxs)(M.KF, {
                                display: 'flex',
                                justifyContent: 'space-between',
                                children: [
                                  (0, o.jsxs)(v.xu, {
                                    display: 'flex',
                                    children: [
                                      (0, o.jsx)(M.XE, { marginRight: 2 }),
                                      (0, o.jsx)(Z.X, { size: 'sm', children: 'Physics' }),
                                    ],
                                  }),
                                  (0, o.jsx)(A.r, {
                                    id: 'physicsOn',
                                    onChange: function () {
                                      return t(U(U({}, n), {}, { enabled: !n.enabled }))
                                    },
                                    isChecked: n.enabled,
                                    colorScheme: 'purple',
                                  }),
                                ],
                              }),
                              (0, o.jsxs)(M.Hk, {
                                children: [
                                  (0, o.jsxs)(R.gC, {
                                    spacing: 2,
                                    justifyContent: 'flex-start',
                                    divider: (0, o.jsx)(R.cX, { borderColor: 'gray.500' }),
                                    align: 'stretch',
                                    paddingLeft: 7,
                                    color: 'gray.800',
                                    children: [
                                      (0, o.jsx)(G, {
                                        label: 'Gravity',
                                        value: n.gravityOn,
                                        onChange: function () {
                                          return t(U(U({}, n), {}, { gravityOn: !n.gravityOn }))
                                        },
                                        children: (0, o.jsx)(Y, {
                                          label: 'Strength',
                                          value: 10 * n.gravity,
                                          onChange: function (e) {
                                            return t(U(U({}, n), {}, { gravity: e / 10 }))
                                          },
                                        }),
                                      }),
                                      (0, o.jsx)(Y, {
                                        value: -n.charge / 100,
                                        onChange: function (e) {
                                          return t(U(U({}, n), {}, { charge: -100 * e }))
                                        },
                                        label: 'Repulsive Force',
                                      }),
                                      (0, o.jsx)(G, {
                                        label: 'Collision',
                                        infoText: 'Perfomance sap, disable if slow',
                                        value: n.collision,
                                        onChange: function () {
                                          return t(U(U({}, n), {}, { collision: !n.collision }))
                                        },
                                        children: (0, o.jsx)(Y, {
                                          value: 10 * n.collisionStrength,
                                          onChange: function (e) {
                                            return t(U(U({}, n), {}, { collisionStrength: e / 10 }))
                                          },
                                          label: 'Strength',
                                        }),
                                      }),
                                      (0, o.jsx)(Y, {
                                        value: 5 * n.linkStrength,
                                        onChange: function (e) {
                                          return t(U(U({}, n), {}, { linkStrength: e / 5 }))
                                        },
                                        label: 'Link Force',
                                      }),
                                      (0, o.jsx)(Y, {
                                        label: 'Link Iterations',
                                        value: n.linkIts,
                                        onChange: function (e) {
                                          return t(U(U({}, n), {}, { linkIts: e }))
                                        },
                                        min: 0,
                                        max: 6,
                                        step: 1,
                                        infoText:
                                          'How many links down the line the physics of a single node affects (Slow)',
                                      }),
                                      (0, o.jsx)(Y, {
                                        label: 'Viscosity',
                                        value: 10 * n.velocityDecay,
                                        onChange: function (e) {
                                          return t(U(U({}, n), {}, { velocityDecay: e / 10 }))
                                        },
                                      }),
                                    ],
                                  }),
                                  (0, o.jsx)(v.xu, {
                                    children: (0, o.jsx)(M.UQ, {
                                      allowToggle: !0,
                                      children: (0, o.jsxs)(M.Qd, {
                                        children: [
                                          (0, o.jsxs)(M.KF, {
                                            children: [
                                              (0, o.jsx)(H.x, { children: 'Advanced' }),
                                              (0, o.jsx)(M.XE, { marginRight: 2 }),
                                            ],
                                          }),
                                          (0, o.jsx)(M.Hk, {
                                            children: (0, o.jsxs)(R.gC, {
                                              spacing: 2,
                                              justifyContent: 'flex-start',
                                              divider: (0, o.jsx)(R.cX, {
                                                borderColor: 'gray.500',
                                              }),
                                              align: 'stretch',
                                              paddingLeft: 3,
                                              color: 'gray.800',
                                              children: [
                                                (0, o.jsx)(Y, {
                                                  label: 'Iterations per tick',
                                                  min: 1,
                                                  max: 10,
                                                  step: 1,
                                                  value: n.iterations,
                                                  onChange: function (e) {
                                                    return t(U(U({}, n), {}, { iterations: e }))
                                                  },
                                                  infoText:
                                                    'Number of times the physics simulation iterates per simulation step',
                                                }),
                                                (0, o.jsx)(Y, {
                                                  label: 'Stabilization rate',
                                                  value: 50 * n.alphaDecay,
                                                  onChange: function (e) {
                                                    return t(
                                                      U(U({}, n), {}, { alphaDecay: e / 50 }),
                                                    )
                                                  },
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
                          (0, o.jsxs)(M.Qd, {
                            children: [
                              (0, o.jsxs)(M.KF, {
                                children: [
                                  (0, o.jsx)(M.XE, { marginRight: 2 }),
                                  (0, o.jsx)(Z.X, { size: 'sm', children: 'Visual' }),
                                ],
                              }),
                              (0, o.jsx)(M.Hk, {
                                children: (0, o.jsxs)(R.gC, {
                                  spacing: 2,
                                  justifyContent: 'flex-start',
                                  divider: (0, o.jsx)(R.cX, { borderColor: 'gray.500' }),
                                  align: 'stretch',
                                  paddingLeft: 7,
                                  color: 'gray.800',
                                  children: [
                                    (0, o.jsx)(G, {
                                      label: 'Colors',
                                      onChange: function () {
                                        return t(U(U({}, n), {}, { colorful: !n.colorful }))
                                      },
                                      value: n.colorful,
                                      children: (0, o.jsx)(H.x, { children: 'Child' }),
                                    }),
                                    (0, o.jsx)(Y, {
                                      label: 'Node size',
                                      value: n.nodeRel,
                                      onChange: function (e) {
                                        return t(U(U({}, n), {}, { nodeRel: e }))
                                      },
                                    }),
                                    (0, o.jsx)(Y, {
                                      label: 'Link width',
                                      value: n.linkWidth,
                                      onChange: function (e) {
                                        return t(U(U({}, n), {}, { linkWidth: e }))
                                      },
                                    }),
                                    (0, o.jsxs)(v.xu, {
                                      children: [
                                        (0, o.jsxs)(X.k, {
                                          alignItems: 'center',
                                          justifyContent: 'space-between',
                                          children: [
                                            (0, o.jsx)(H.x, { children: 'Labels' }),
                                            (0, o.jsxs)(_.v2, {
                                              children: [
                                                (0, o.jsx)(_.j2, {
                                                  as: L.z,
                                                  rightIcon: (0, o.jsx)(P.v, {}),
                                                  children: n.labels
                                                    ? n.labels < 2
                                                      ? 'On Highlight'
                                                      : 'Always'
                                                    : 'Never',
                                                }),
                                                (0, o.jsxs)(_.qy, {
                                                  bgColor: 'gray.200',
                                                  children: [
                                                    (0, o.jsx)(_.sN, {
                                                      onClick: function () {
                                                        return t(U(U({}, n), {}, { labels: 0 }))
                                                      },
                                                      children: 'Never',
                                                    }),
                                                    (0, o.jsx)(_.sN, {
                                                      onClick: function () {
                                                        return t(U(U({}, n), {}, { labels: 1 }))
                                                      },
                                                      children: 'On Highlight',
                                                    }),
                                                    (0, o.jsx)(_.sN, {
                                                      onClick: function () {
                                                        return t(U(U({}, n), {}, { labels: 2 }))
                                                      },
                                                      children: 'Always',
                                                    }),
                                                  ],
                                                }),
                                              ],
                                            }),
                                          ],
                                        }),
                                        (0, o.jsx)(E.U, {
                                          in: n.labels > 1,
                                          animateOpacity: !0,
                                          children: (0, o.jsx)(v.xu, {
                                            paddingLeft: 4,
                                            paddingTop: 2,
                                            children: (0, o.jsx)(Y, {
                                              label: 'Label Appearance Scale',
                                              value: 5 * n.labelScale,
                                              onChange: function (e) {
                                                return t(U(U({}, n), {}, { labelScale: e / 5 }))
                                              },
                                            }),
                                          }),
                                        }),
                                      ],
                                    }),
                                    (0, o.jsxs)(G, {
                                      label: 'Directional Particles',
                                      value: n.particles,
                                      onChange: function () {
                                        return t(U(U({}, n), {}, { particles: !n.particles }))
                                      },
                                      children: [
                                        (0, o.jsx)(Y, {
                                          label: 'Particle Number',
                                          value: n.particlesNumber,
                                          max: 5,
                                          step: 1,
                                          onChange: function (e) {
                                            return t(U(U({}, n), {}, { particlesNumber: e }))
                                          },
                                        }),
                                        (0, o.jsx)(Y, {
                                          label: 'Particle Size',
                                          value: n.particlesWidth,
                                          onChange: function (e) {
                                            return t(U(U({}, n), {}, { particlesWidth: e }))
                                          },
                                        }),
                                      ],
                                    }),
                                    (0, o.jsx)(G, {
                                      label: 'Highlight',
                                      onChange: function () {
                                        return t(U(U({}, n), {}, { highlight: !n.highlight }))
                                      },
                                      value: n.highlight,
                                      children: (0, o.jsxs)(R.gC, {
                                        spacing: 1,
                                        justifyContent: 'flex-start',
                                        divider: (0, o.jsx)(R.cX, { borderColor: 'gray.400' }),
                                        align: 'stretch',
                                        paddingLeft: 0,
                                        children: [
                                          (0, o.jsx)(Y, {
                                            label: 'Highlight Link Thickness',
                                            value: n.highlightLinkSize,
                                            onChange: function (e) {
                                              return t(U(U({}, n), {}, { highlightLinkSize: e }))
                                            },
                                          }),
                                          (0, o.jsx)(Y, {
                                            label: 'Highlight Node Size',
                                            value: n.highlightNodeSize,
                                            onChange: function (e) {
                                              return t(U(U({}, n), {}, { highlightNodeSize: e }))
                                            },
                                          }),
                                          (0, o.jsxs)(G, {
                                            label: 'Highlight Animation',
                                            onChange: function () {
                                              t(
                                                U(
                                                  U({}, n),
                                                  {},
                                                  { highlightAnim: !n.highlightAnim },
                                                ),
                                              )
                                            },
                                            value: n.highlightAnim,
                                            children: [
                                              (0, o.jsx)(Y, {
                                                label: 'Animation speed',
                                                onChange: function (e) {
                                                  return t(U(U({}, n), {}, { animationSpeed: e }))
                                                },
                                                value: n.animationSpeed,
                                                infoText:
                                                  'Slower speed has a chance of being buggy',
                                                min: 50,
                                                max: 1e3,
                                                step: 10,
                                              }),
                                              (0, o.jsx)(W.Ph, {
                                                placeholder: n.algorithmName,
                                                onChange: function (e) {
                                                  t(
                                                    U(
                                                      U({}, n),
                                                      {},
                                                      { algorithmName: e.target.value },
                                                    ),
                                                  )
                                                },
                                                children: n.algorithmOptions.map(function (e) {
                                                  return (0,
                                                  o.jsx)('option', { value: e, children: e }, e)
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
                          }),
                          (0, o.jsxs)(M.Qd, {
                            children: [
                              (0, o.jsxs)(M.KF, {
                                children: [
                                  (0, o.jsx)(M.XE, { marginRight: 2 }),
                                  (0, o.jsx)(Z.X, { size: 'sm', children: 'Behavior' }),
                                ],
                              }),
                              (0, o.jsx)(M.Hk, {
                                children: (0, o.jsx)(R.gC, {
                                  spacing: 2,
                                  justifyContent: 'flex-start',
                                  divider: (0, o.jsx)(R.cX, { borderColor: 'gray.500' }),
                                  align: 'stretch',
                                  paddingLeft: 7,
                                  color: 'gray.800',
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
        V = function (e) {
          var n = e.infoText
          return (0, o.jsx)(v.xu, {
            paddingLeft: '1',
            children: (0, o.jsx)(F.u, {
              label: n,
              placement: 'top',
              color: 'gray.100',
              bg: 'gray.800',
              hasArrow: !0,
              children: (0, o.jsx)(T.h, {}),
            }),
          })
        },
        Y = function (e) {
          var n = e.min,
            t = void 0 === n ? 0 : n,
            r = e.max,
            i = void 0 === r ? 10 : r,
            l = e.step,
            a = void 0 === l ? 0.1 : l,
            s = e.value,
            c = void 0 === s ? 1 : s,
            u = (0, w.Z)(e, ['min', 'max', 'step', 'value']),
            d = u.onChange,
            h = u.label,
            f = u.infoText
          return (0, o.jsxs)(v.xu, {
            children: [
              (0, o.jsxs)(v.xu, {
                display: 'flex',
                alignItems: 'flex-end',
                children: [(0, o.jsx)(H.x, { children: h }), f && (0, o.jsx)(V, { infoText: f })],
              }),
              (0, o.jsxs)(B.iR, {
                value: c,
                onChange: d,
                min: t,
                max: i,
                step: a,
                colorScheme: 'purple',
                children: [
                  (0, o.jsx)(B.Uj, { children: (0, o.jsx)(B.Ms, {}) }),
                  (0, o.jsx)(F.u, {
                    bg: 'purple.500',
                    label: c.toFixed(1),
                    children: (0, o.jsx)(B.gs, { bg: 'white' }),
                  }),
                ],
              }),
            ],
          })
        },
        G = function (e) {
          var n = e.value,
            t = e.onChange,
            r = e.label,
            i = e.infoText,
            l = e.children
          return (0, o.jsxs)(v.xu, {
            children: [
              (0, o.jsxs)(v.xu, {
                display: 'flex',
                justifyContent: 'space-between',
                children: [
                  (0, o.jsxs)(v.xu, {
                    display: 'flex',
                    alignItems: 'center',
                    children: [
                      (0, o.jsx)(H.x, { children: r }),
                      i && (0, o.jsx)(V, { infoText: i }),
                    ],
                  }),
                  (0, o.jsx)(A.r, { isChecked: !!n, onChange: t, colorScheme: 'purple' }),
                ],
              }),
              (0, o.jsx)(E.U, {
                in: !!n,
                animateOpacity: !0,
                children: (0, o.jsx)(v.xu, { paddingLeft: 4, paddingTop: 2, children: l }),
              }),
            ],
          })
        }
      function q(e, n) {
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
      function $(e) {
        for (var n = 1; n < arguments.length; n++) {
          var t = null != arguments[n] ? arguments[n] : {}
          n % 2
            ? q(Object(t), !0).forEach(function (n) {
                ;(0, s.Z)(e, n, t[n])
              })
            : Object.getOwnPropertyDescriptors
            ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t))
            : q(Object(t)).forEach(function (n) {
                Object.defineProperty(e, n, Object.getOwnPropertyDescriptor(t, n))
              })
        }
        return e
      }
      var ee = t.e(4).then(t.bind(t, 7004)),
        ne = t.g.window ? t(1957).f$ : null,
        te = t.g.window ? t(1957).s6 : null
      function re() {
        var e = (0, u.useState)(!1),
          n = e[0],
          t = e[1]
        return (
          (0, u.useEffect)(function () {
            t(!0)
          }, []),
          n ? (0, o.jsx)(ie, {}) : null
        )
      }
      function ie() {
        var e = d('physics', S),
          n = (0, c.Z)(e, 2),
          t = n[0],
          r = n[1],
          i = d('filter', O),
          l = (0, c.Z)(i, 2),
          h = l[0],
          f = l[1],
          g = (0, u.useState)(null),
          p = g[0],
          x = g[1],
          j = (0, u.useState)(null),
          m = j[0],
          y = j[1],
          b = (0, u.useRef)({}),
          k = (0, u.useRef)({}),
          C = function () {
            return fetch('http://localhost:35901/graph')
              .then(function (e) {
                return e.json()
              })
              .then(function (e) {
                var n = e.nodes.reduce(function (e, n) {
                    var t
                    return $(
                      $({}, e),
                      {},
                      (0, s.Z)(
                        {},
                        n.file,
                        [].concat((0, a.Z)(null !== (t = e[n.file]) && void 0 !== t ? t : []), [n]),
                      ),
                    )
                  }, {}),
                  t = Object.keys(n).flatMap(function (e) {
                    var t,
                      r = null !== (t = n[e]) && void 0 !== t ? t : [],
                      i = r.find(function (e) {
                        return 0 === e.level
                      }),
                      l = r.filter(function (e) {
                        return 0 !== e.level
                      })
                    return i
                      ? l.map(function (e) {
                          return { source: e.id, target: i.id, type: 'parent' }
                        })
                      : []
                  })
                b.current = Object.fromEntries(
                  e.nodes.map(function (e) {
                    return [e.id, e]
                  }),
                )
                var r = [].concat((0, a.Z)(e.links), (0, a.Z)(t))
                k.current = r.reduce(function (e, n) {
                  var t, r, i
                  return $(
                    $({}, e),
                    {},
                    ((i = {}),
                    (0, s.Z)(
                      i,
                      n.source,
                      [].concat((0, a.Z)(null !== (t = e[n.source]) && void 0 !== t ? t : []), [n]),
                    ),
                    (0, s.Z)(
                      i,
                      n.target,
                      [].concat((0, a.Z)(null !== (r = e[n.target]) && void 0 !== r ? r : []), [n]),
                    ),
                    i),
                  )
                }, {})
                var i = $($({}, e), {}, { links: r }),
                  l = JSON.parse(JSON.stringify(i))
                x(l)
              })
          }
        ;(0, u.useEffect)(function () {
          new EventSource('http://127.0.0.1:35901/current-node-id').addEventListener(
            'message',
            function (e) {
              var n = e.data
              y(n)
            },
          ),
            C()
        }, []),
          (0, u.useEffect)(
            function () {
              m && C()
            },
            [m],
          )
        var w = (0, u.useState)(!1),
          N = w[0],
          I = w[1]
        return p
          ? (0, o.jsxs)(v.xu, {
              display: 'flex',
              alignItems: 'flex-start',
              flexDirection: 'row',
              height: '100%',
              children: [
                (0, o.jsx)(
                  K,
                  $(
                    {},
                    {
                      physics: t,
                      setPhysics: r,
                      threeDim: N,
                      setThreeDim: I,
                      filter: h,
                      setFilter: f,
                    },
                  ),
                ),
                (0, o.jsx)(v.xu, {
                  position: 'absolute',
                  alignItems: 'top',
                  children: (0, o.jsx)(
                    le,
                    $(
                      { nodeById: b.current, linksByNodeId: k.current },
                      { physics: t, graphData: p, threeDim: N, emacsNodeId: m, filter: h },
                    ),
                  ),
                }),
              ],
            })
          : null
      }
      var le = function (e) {
        var n = e.physics,
          t = e.graphData,
          r = e.threeDim,
          s = e.linksByNodeId,
          d = e.filter,
          h = e.emacsNodeId,
          v = e.nodeById,
          j = (0, u.useRef)(null),
          m = (0, u.useRef)(null),
          y = (0, g.iP)(),
          b = (0, c.Z)(y, 2),
          k = b[0],
          C = b[1],
          S = (0, u.useState)(null),
          O = S[0],
          w = S[1],
          N = (0, u.useState)({ nodeIds: [] }),
          I = N[0],
          D = N[1]
        ;(0, u.useEffect)(
          function () {
            if (h)
              switch (n.follow) {
                case 'Local':
                  D({ nodeIds: [h] })
              }
          },
          [h],
        )
        var P = O,
          T = (0, u.useMemo)(
            function () {
              if (!P) return {}
              var e = s[P.id]
              return e
                ? Object.fromEntries(
                    [P.id]
                      .concat(
                        (0, a.Z)(
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
            [P, s],
          ),
          z = (0, u.useMemo)(
            function () {
              return t.nodes.filter(function (e) {
                var n,
                  t = null !== (n = s[e.id]) && void 0 !== n ? n : [],
                  r = !0
                return (
                  d.orphans &&
                    (d.parents
                      ? (r = 0 !== t.length)
                      : (0 === t.length ||
                          t.length -
                            t.filter(function (e) {
                              return 'parent' === e.type || 'cite' === e.type
                            }).length ===
                            0) &&
                        (r = !1)),
                  r
                )
              })
            },
            [d, t.nodes, s],
          ),
          E = (0, u.useMemo)(
            function () {
              return t.links.filter(function (e) {
                var n = e
                return 'cite' !== n.type && (d.parents || 'parent' !== n.type)
              })
            },
            [d, JSON.stringify(t.links)],
          ),
          F = (0, u.useMemo)(
            function () {
              return z.filter(function (e) {
                var n,
                  t = null !== (n = s[e.id]) && void 0 !== n ? n : []
                return (
                  I.nodeIds.includes(e.id) ||
                  t.some(function (e) {
                    return I.nodeIds.includes(e.source) || I.nodeIds.includes(e.target)
                  })
                )
              })
            },
            [z, s, I.nodeIds],
          ),
          L = F.map(function (e) {
            return e.id
          }),
          M = (0, u.useMemo)(
            function () {
              return E.filter(function (e) {
                var n = 'object' === typeof e.source ? e.source.id : e.source,
                  t = 'object' === typeof e.target ? e.target.id : e.target
                return L.includes(n) && L.includes(t)
              })
            },
            [E, F],
          ),
          Z = (0, u.useMemo)(
            function () {
              return 0 === I.nodeIds.length ? { nodes: z, links: E } : { nodes: F, links: M }
            },
            [d, I, JSON.stringify(Object.keys(v))],
          )
        ;(0, u.useEffect)(
          function () {
            setTimeout(function () {
              var e = r ? m.current : j.current
              null === e || void 0 === e || e.zoomToFit(0, ae(20, 200, k / 8))
            }, 1)
          },
          [JSON.stringify(L)],
        ),
          (0, u.useEffect)(function () {
            ;(0, l.Z)(
              i().mark(function e() {
                var t, l
                return i().wrap(function (e) {
                  for (;;)
                    switch ((e.prev = e.next)) {
                      case 0:
                        return (t = r ? m.current : j.current), (e.next = 3), ee
                      case 3:
                        ;(l = e.sent),
                          n.gravityOn
                            ? (t.d3Force('x', l.forceX().strength(n.gravity)),
                              t.d3Force('y', l.forceY().strength(n.gravity)),
                              r
                                ? n.galaxy
                                  ? (t.d3Force('x', l.forceX().strength(n.gravity / 5)),
                                    t.d3Force('z', l.forceZ().strength(n.gravity / 5)))
                                  : (t.d3Force('x', l.forceX().strength(n.gravity)),
                                    t.d3Force('z', l.forceZ().strength(n.gravity)))
                                : t.d3Force('z', null))
                            : (t.d3Force('x', null),
                              t.d3Force('y', null),
                              r && t.d3Force('z', null)),
                          n.linkStrength && t.d3Force('link').strength(n.linkStrength),
                          n.linkIts && t.d3Force('link').iterations(n.linkIts),
                          n.charge && t.d3Force('charge').strength(n.charge),
                          t.d3Force('collide', n.collision ? l.forceCollide().radius(20) : null)
                      case 9:
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
              null === (e = j.current) || void 0 === e || e.d3ReheatSimulation()
            },
            [n],
          )
        var R = (0, u.useRef)(0),
          X = (0, u.useState)(1),
          H = X[0],
          A = X[1],
          _ = (0, p._7)(
            function (e) {
              return A(e)
            },
            { duration: n.animationSpeed, algorithm: n.algorithms[n.algorithmName] },
          ),
          W = (0, c.Z)(_, 2),
          B = W[0],
          J = W[1],
          Q = (0, p._7)(
            function (e) {
              return A(Math.min(H, -1 * (e - 1)))
            },
            { duration: n.animationSpeed, algorithm: n.algorithms[n.algorithmName] },
          ),
          U = (0, c.Z)(Q, 1)[0],
          K = (0, u.useRef)(null)
        ;(0, u.useEffect)(
          function () {
            if ((O && (K.current = O), !n.highlightAnim)) return A(O ? 1 : 0)
            O ? B() : (J(), H > 0.5 ? U() : A(0))
          },
          [O],
        )
        var V = (0, x.useTheme)(),
          Y = (0, u.useMemo)(
            function () {
              return f.Z(V.colors.gray[500], V.colors.purple[500])
            },
            [V],
          ),
          G = (0, u.useMemo)(
            function () {
              return f.Z(V.colors.gray[500], V.colors.gray[400])
            },
            [V],
          ),
          q =
            ((0, u.useMemo)(
              function () {
                var e
                return null !== (e = s[null === O || void 0 === O ? void 0 : O.id]) && void 0 !== e
                  ? e
                  : []
              },
              [O],
            ),
            (0, u.useMemo)(
              function () {
                var e, n
                return null !== (e = s[null === (n = K.current) || void 0 === n ? void 0 : n.id]) &&
                  void 0 !== e
                  ? e
                  : []
              },
              [O],
            )),
          re = (0, u.useMemo)(
            function () {
              var e
              return Object.fromEntries(
                [null === (e = K.current) || void 0 === e ? void 0 : e.id]
                  .concat(
                    (0, a.Z)(
                      q.flatMap(function (e) {
                        return [e.source, e.target]
                      }),
                    ),
                  )
                  .map(function (e) {
                    return [e, {}]
                  }),
              )
            },
            [O, q, K],
          ),
          ie = {
            graphData: Z,
            width: k,
            height: C,
            backgroundColor: V.white,
            nodeLabel: function (e) {
              return e.title
            },
            nodeColor: function (e) {
              var t, r, i
              if (!n.colorful) return re[e.id] || T[e.id] ? Y(H) : G(H)
              if (e.id === h) return V.colors.red[500]
              var l = [
                  'pink',
                  'purple',
                  'blue',
                  'cyan',
                  'teal',
                  'green',
                  'yellow',
                  'orange',
                  'red',
                ].filter(function (e) {
                  return !['red'].includes(e)
                }),
                o =
                  null !== (t = null === (r = s[e.id]) || void 0 === r ? void 0 : r.length) &&
                  void 0 !== t
                    ? t
                    : 0,
                a = o
                  ? null === (i = s[e.id]) || void 0 === i
                    ? void 0
                    : i.filter(function (e) {
                        return 'parent' === e.type || 'cite' === e.type
                      }).length
                  : 0,
                c = d.parents ? o : o - a
              return V.colors[l[ae(c, 0, l.length - 1)]][500]
            },
            nodeRelSize: n.nodeRel,
            nodeVal: function (e) {
              var t,
                r = null !== (t = s[e.id]) && void 0 !== t ? t : [],
                i = r.length
                  ? r.filter(function (e) {
                      return 'parent' === e.type || 'cite' === e.type
                    }).length
                  : 0
              return (
                (3 + r.length - (d.parents ? 0 : i)) *
                (T[e.id] || re[e.id] ? 1 + H * (n.highlightNodeSize - 1) : 1)
              )
            },
            nodeCanvasObject: function (e, t, r) {
              var i
              if (e && n.labels) {
                i = s[e.id]
                var l = re[e.id]
                if (!(r <= n.labelScale || 1 === n.labels) || T[e.id] || l) {
                  var o = e.title,
                    c = o.substring(0, Math.min(o.length, 30)),
                    u = 12 / r,
                    d = [1.1 * t.measureText(c).width, u].map(function (e) {
                      return e + 0.5 * u
                    }),
                    h = Math.min((3 * (r - n.labelScale)) / n.labelScale, 1),
                    f = function () {
                      return 1 === n.labels || r <= n.labelScale
                        ? H
                        : T[e.id] || re[e.id]
                        ? Math.max(h, H)
                        : 1 * h * (-1 * (0.5 * H - 1))
                    }
                  if (2 === n.labels && (l || T[e.id])) {
                    var g = 0.5 * f()
                    ;(t.fillStyle = 'rgba(20, 20, 20, '.concat(g, ')')),
                      t.fillRect.apply(t, [e.x - d[0] / 2, e.y - d[1] / 2].concat((0, a.Z)(d)))
                  }
                  var p = f()
                  ;(t.textAlign = 'center'),
                    (t.textBaseline = 'middle'),
                    (t.fillStyle = 'rgb(255, 255, 255, '.concat(p, ')')),
                    (t.font = ''.concat(u, 'px Sans-Serif')),
                    t.fillText(c, e.x, e.y)
                }
              }
            },
            nodeCanvasObjectMode: function () {
              return 'after'
            },
            linkDirectionalParticles: n.particles ? n.particlesNumber : void 0,
            linkColor: function (e) {
              var n = oe(e, P),
                t = oe(e, K.current)
              return n || t ? Y(H) : V.colors.gray[500]
            },
            linkWidth: function (e) {
              var t = oe(e, P),
                r = oe(e, K.current)
              return t || r ? n.linkWidth * (1 + H * (n.highlightLinkSize - 1)) : n.linkWidth
            },
            linkDirectionalParticleWidth: n.particlesWidth,
            d3AlphaDecay: n.alphaDecay,
            d3AlphaMin: n.alphaMin,
            d3VelocityDecay: n.velocityDecay,
            onNodeClick: function (e, n) {
              var t = n.timeStamp - R.current < 400
              ;(R.current = n.timeStamp),
                t
                  ? window.open('org-protocol://roam-node?node=' + e.id, '_self')
                  : D(function (n) {
                      return $($({}, n), {}, { nodeIds: [].concat((0, a.Z)(n.nodeIds), [e.id]) })
                    })
            },
            onBackgroundClick: function () {
              D(function (e) {
                return $($({}, e), {}, { nodeIds: [] })
              })
            },
            onNodeHover: function (e) {
              n.hover && w(e)
            },
          }
        return (0, o.jsx)('div', {
          children: r
            ? (0, o.jsx)(
                te,
                $(
                  $({ ref: m }, ie),
                  {},
                  { nodeThreeObjectExtend: !0, backgroundColor: V.colors.white },
                ),
              )
            : (0, o.jsx)(ne, $({ ref: j }, ie)),
        })
      }
      function oe(e, n) {
        return (
          e.source.id === (null === n || void 0 === n ? void 0 : n.id) ||
          e.target.id === (null === n || void 0 === n ? void 0 : n.id)
        )
      }
      function ae(e, n, t) {
        return Math.min(Math.max(e, n), t)
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
    e.O(0, [774, 737, 446, 611, 888, 179], function () {
      return (n = 5301), e((e.s = n))
      var n
    })
    var n = e.O()
    _N_E = n
  },
])
