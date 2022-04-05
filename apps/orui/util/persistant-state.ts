import { useState, useEffect } from 'react'

export function usePersistantState<V>(
  storageKey: string,
  defaultValue: V,
  options: {
    storage?: Storage
  } = {},
): [V, (v: V | ((v: V) => V)) => void] {
  const storage = getStorage<V | undefined>(storageKey, options.storage ?? localStorage)

  const storageValue = storage.get()
  const calculatedDefaultValue = storageValue !== undefined ? storageValue : defaultValue
  const calculatedDefaultValueObject =
    storageValue != null &&
    typeof storageValue === 'object' &&
    Array.isArray(storageValue) === false
      ? { ...defaultValue, ...storageValue }
      : calculatedDefaultValue
  if (calculatedDefaultValueObject !== storageValue) {
    storage.update(calculatedDefaultValueObject)
  }
  const [value, setValue] = useState<V>(calculatedDefaultValueObject)

  // change state gracefully when changing the storageKey
  useEffect(() => {
    if (value !== calculatedDefaultValueObject) {
      setValue(calculatedDefaultValueObject)
    }
  }, [storageKey])
  const set = (newValueOrFn: V | ((v: V) => V)) => {
    if (newValueOrFn instanceof Function) {
      setValue((oldValue) => {
        const newValue = newValueOrFn(oldValue)
        storage.update(newValue)
        return newValue
      })
      return
    }
    setValue(newValueOrFn)
    storage.update(newValueOrFn)
  }

  return [value, set]
}

export function getStorage<T>(key: string, storage: Storage) {
  return {
    get(): T | undefined {
      const value = storage.getItem(key)
      if (value && value !== 'undefined') {
        return JSON.parse(value)
      }

      return undefined
    },
    update(updatedState: T) {
      storage.setItem(key, JSON.stringify(updatedState))
    },
    remove() {
      storage.removeItem(key)
    },
  }
}
