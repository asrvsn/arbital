export const authSuccess = (session) => ({
  type: 'AUTH_SUCCESS',
  session
})

export const registerReloadListener = (name, listener) => ({
  type: 'REGISTER_RELOAD_LISTENER',
  name,
  listener
})

export const unregisterReloadListener = (name) => ({
  type: 'UNREGISTER_RELOAD_LISTENER',
  name
})

export const fireReload = (dataSource) => ({
  type: 'FIRE_RELOAD',
  dataSource
})

export const pushDialog = ({dialogType, props}) => ({
  type: 'INTERACT_DIALOG_STACK',
  pushState: {action: 'PUSH', dialogType, props}
})

export const popDialog = ({dialogType, props}) => ({
  type: 'INTERACT_DIALOG_STACK',
  pushState: {action: 'POP', dialogType, props}
})
