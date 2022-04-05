import { createContext } from 'react'

export interface NoteContextProps {
  outline: boolean
  collapse: boolean
}

export const NoteContext = createContext<NoteContextProps>({
  outline: false,
  collapse: true,
})
