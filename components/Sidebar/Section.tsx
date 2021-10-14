import { Box, Collapse, Flex, IconButton } from '@chakra-ui/react'
import React, { JSXElementConstructor, ReactChild, ReactElement, ReactNode, useState } from 'react'
import { BiChevronDownCircle, BiCircle } from 'react-icons/bi'
import { ComponentLike, ComponentPropsWithoutNode } from 'rehype-react'

export interface SectionProps {
  children: any
  className: string
  indent: boolean
}
export const Section = (
  props: SectionProps,
): ComponentLike<
  ReactElement<{}, string | JSXElementConstructor<any>>,
  ComponentPropsWithoutNode
> => {
  const { children, className, indent } = props
  const [open, setOpen] = useState(true)
  if (className === 'h0Wrapper headingWrapper') {
    return children
  }
  const kids = children as ReactChild[]
  return (
    <Box className={'sec'}>
      <Box display="block" pt={2}>
        <Flex alignItems="center">
          {open ? (
            <IconButton
              aria-label="Expand heading"
              mr={1}
              size="xs"
              variant="subtle"
              icon={<BiCircle />}
              onClick={() => setOpen(!open)}
            />
          ) : (
            <IconButton
              aria-label="Collapse heading"
              mr={1}
              size="xs"
              variant="subtle"
              icon={<BiChevronDownCircle />}
              onClick={() => setOpen(!open)}
            />
          )}
          {kids[0]}
        </Flex>
      </Box>
      <Collapse in={open} animateOpacity={false}>
        {indent ? (
          <Box pt={2} pl={4} ml={3} borderLeftWidth="1px" borderLeftColor="purple.500">
            {kids.slice(1)}
          </Box>
        ) : (
          kids.slice(1)
        )}
      </Collapse>
    </Box>
  )
}
