import {
  Button,
  VStack
} from '@chakra-ui/react'
import { saveStorage } from '../../../util/webSocketFunctions'

export interface StoragePanelProps {
  webSocket: any
}

export const StoragePanel = (props: StoragePanelProps) => {
  const { webSocket } = props
  return (
    <VStack>
      <Button onClick={() => {
        saveStorage(JSON.stringify(localStorage), webSocket)
      }}>Save Settings</Button>
      <Button onClick={() => {
        fetch(`http://localhost:35901/settings`)
          .then((res) => {
            return res.text()
          })
          .then((res) =>{
            const settings = JSON.parse(res)
            localStorage.clear()
            for (const key in settings) {
              localStorage.setItem(key, settings[key])
            }
            location.reload()
          })
          .catch((e) => {
            console.log(e)
            return 'Could not fetch the settings for some reason, sorry!'
          })
      }}>Load Settings</Button>
    </VStack> 
  )
}