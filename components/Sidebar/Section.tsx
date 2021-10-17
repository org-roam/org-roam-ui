import { Box, Collapse, Flex, IconButton } from '@chakra-ui/react'
import React, { JSXElementConstructor, ReactChild, ReactElement, ReactNode, useState } from 'react'
import { BiCaretDownCircle, BiChevronDownCircle, BiCircle } from 'react-icons/bi'
import { ComponentLike, ComponentPropsWithoutNode } from 'rehype-react'
import { VscCircleFilled, VscCircleOutline } from 'react-icons/vsc'

export interface SectionProps {
  children: any
  className: string
  //outline: boolean
}

export const Section = (props: SectionProps) => {
  const {
    children,
    className, // outline
  } = props
  const outline = true
  const [open, setOpen] = useState(true)
  if (className === 'h0Wrapper headingWrapper') {
    return <Box> {children}</Box>
  }
  const kids = children as ReactChild[]
  return (
    <Box className={'sec'}>
      <Box display="block" pt={2}>
        <Flex alignItems="center">
          {open ? (
            <IconButton
              // ml={-7}
              _focus={{}}
              aria-label="Expand heading"
              mr={1}
              size="xs"
              variant="subtle"
              icon={<VscCircleOutline />}
              onClick={() => setOpen(!open)}
            />
          ) : (
            <IconButton
              // ml={-7}
              _focus={{}}
              aria-label="Collapse heading"
              mr={1}
              size="xs"
              variant="subtle"
              icon={<VscCircleFilled />}
              onClick={() => setOpen(!open)}
            />
          )}
          {kids[0]}
        </Flex>
      </Box>
      {open && (
        <>
          {outline ? (
            <Box pt={2} pl={4} ml={3} borderLeftWidth="1px" borderLeftColor="purple.500">
              {kids.slice(1)}
            </Box>
          ) : (
            kids.slice(1)
          )}
        </>
      )}
    </Box>
  )
}
