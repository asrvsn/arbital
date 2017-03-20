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
        console.warn('FIRE_RELOAD', action.dataSource)
        // XXX (anand) this does effectful things. Figure out a better way.
        Object.values(state).forEach(
          listener => listener(action.dataSource)
        )
        return state
      }
      default: {
        return state
      }
    }
  }
}
