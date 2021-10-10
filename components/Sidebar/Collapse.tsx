// modified from https://github.com/chakra-ui/chakra-ui/blob/fc3b97d0978cf2adb9fc79157c6e42b4b68155c5/packages/transition/src/collapse.tsx

import { cx, mergeWith, warn, __DEV__ } from '@chakra-ui/utils'
import { AnimatePresence, HTMLMotionProps, motion, Variants as _Variants } from 'framer-motion'
import * as React from 'react'
import { TransitionEasings, Variants, withDelay, WithTransitionConfig } from './transition-utils'

const isNumeric = (value?: string | number) => value != null && parseInt(value.toString(), 10) > 0

export interface CollapseOptions {
  /**
   * If `true`, the opacity of the content will be animated
   * @default true
   */
  animateOpacity?: boolean
  /**
   * The size you want the content in its collapsed state.
   * @default 0
   */
  startingSize?: number | string
  /**
   * The size you want the content in its expanded state.
   * @default "auto"
   */
  endingSize?: number | string
  /**
   * The dimension you want to collapse by.
   * @default "size"
   */
  dimension?: string
}

const defaultTransitions = {
  exit: {
    size: { duration: 0.2, ease: TransitionEasings.ease },
    opacity: { duration: 0.3, ease: TransitionEasings.ease },
  },
  enter: {
    size: { duration: 0.3, ease: TransitionEasings.ease },
    opacity: { duration: 0.4, ease: TransitionEasings.ease },
  },
}

const variants: Variants<CollapseOptions> = {
  exit: ({ animateOpacity, startingSize, transition, transitionEnd, delay, dimension }) => ({
    ...(animateOpacity && { opacity: isNumeric(startingSize) ? 1 : 0 }),
    overflow: 'hidden',
    [dimension as string]: startingSize,
    transitionEnd: transitionEnd?.exit,
    transition: transition?.exit ?? withDelay.exit(defaultTransitions.exit, delay),
  }),
  enter: ({ animateOpacity, endingSize, transition, transitionEnd, delay, dimension }) => ({
    ...(animateOpacity && { opacity: 1 }),
    [dimension as string]: endingSize,
    transitionEnd: transitionEnd?.enter,
    transition: transition?.enter ?? withDelay.enter(defaultTransitions.enter, delay),
  }),
}

export type ICollapse = CollapseProps

export interface CollapseProps
  extends WithTransitionConfig<HTMLMotionProps<'div'>>,
    CollapseOptions {}

export const Collapse = React.forwardRef<HTMLDivElement, CollapseProps>((props, ref) => {
  const {
    in: isOpen,
    unmountOnExit,
    animateOpacity = true,
    startingSize = 0,
    endingSize = 'auto',
    dimension = 'height',
    style,
    className,
    transition,
    transitionEnd,
    ...rest
  } = props

  const [mounted, setMounted] = React.useState(false)
  React.useEffect(() => {
    const timeout = setTimeout(() => {
      setMounted(true)
    })
    return () => clearTimeout(timeout)
  }, [])

  /**
   * Warn 🚨: `startingSize` and `unmountOnExit` are mutually exclusive
   *
   * If you specify a starting size, the collapsed needs to be mounted
   * for the size to take effect.
   */
  warn({
    condition: Boolean(startingSize > 0 && unmountOnExit),
    message: `startingSize and unmountOnExit are mutually exclusive. You can't use them together`,
  })

  const hasStartingSize = parseFloat(startingSize.toString()) > 0

  const custom = {
    startingSize,
    endingSize,
    animateOpacity,
    dimension,
    transition: !mounted ? { enter: { duration: 0 } } : transition,
    transitionEnd: mergeWith(transitionEnd, {
      enter: { overflow: 'initial' },
      exit: unmountOnExit
        ? undefined
        : {
            display: hasStartingSize ? 'block' : 'none',
          },
    }),
  }

  const show = unmountOnExit ? isOpen : true
  const animate = isOpen || unmountOnExit ? 'enter' : 'exit'

  return (
    <AnimatePresence initial={false} custom={custom}>
      {show && (
        <motion.div
          ref={ref}
          {...rest}
          className={cx('chakra-collapse', className)}
          style={{
            overflow: 'hidden',
            display: 'block',
            ...style,
          }}
          custom={custom}
          variants={variants as _Variants}
          initial={unmountOnExit ? 'exit' : false}
          animate={animate}
          exit="exit"
        />
      )}
    </AnimatePresence>
  )
})

if (__DEV__) {
  Collapse.displayName = 'Collapse'
}
