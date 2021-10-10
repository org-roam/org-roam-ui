import React from 'react'
import { Box, Button, Flex, Slide, useDisclosure } from '@chakra-ui/react'
import { Collapse } from '../components/Sidebar/Collapse'

export default function Test() {
  const { isOpen, onClose, onOpen, onToggle } = useDisclosure()
  return (
    <>
      <Button onClick={onToggle}>togg</Button>
      <Flex flexDirection="row" justifyContent="space-between" w="100vw">
        <Box color="blue.500" width="100%">
          a
        </Box>
        <Collapse
          animateOpacity={false}
          dimension="width"
          in={isOpen}
          //style={{ position: 'relative' }}
          unmountOnExit
          endingSize={500}
          startingSize={0}
        >
          <Box height="100vh" bgColor="red.500">
            Hey
          </Box>
        </Collapse>
      </Flex>
    </>
  )
}
