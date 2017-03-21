export default {

  isAuthenticated: (state = false, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS': {
        return true
      }
      default: {
        return state
      }
    }
  },

  session: (state = null, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS': {
        return action.session
      }
      default: {
        return state
      }
    }
  },

  dataReloadListeners: (state = {}, action) => {
    const fireListeners = (dataSource) => {
      console.warn('fireListeners', dataSource)
      Object.values(state).forEach(
        listener => listener(dataSource)
      )
    }

    switch(action.type) {

      case 'REGISTER_RELOAD_LISTENER': {
        const { name, listener } = action
        console.warn('REGISTER_RELOAD_LISTENER', name)
        const state_ = Object.assign({}, state)
        state_[name] = listener
        return state_
      }

      case 'UNREGISTER_RELOAD_LISTENER': {
        const { name } = action
        console.warn('UNREGISTER_RELOAD_LISTENER', name)
        const state_ = Object.assign({}, state)
        delete state_[name]
        return state_
      }

      case 'FIRE_RELOAD': {
        // XXX (anand) this does effectful things. Figure out a better way.
        fireListeners(action.datasource)
        return state
      }

      case 'CLOSE_DIALOG': {
        // XXX (anand) this is just plain hardcoded.
        fireListeners("CLAIMS")
        return state
      }

      default: {
        return state
      }
    }
  },

  currentDialogConfig: (state = {dialogType: 'NONE', payload: {}}, action) => {
    switch(action.type) {
      case 'OPEN_DIALOG': {
        return action.config
      }
      case 'CLOSE_DIALOG': {
        return {dialogType: 'NONE', payload: {}}
      }
      default: {
        return state
      }
    }
  }
}
