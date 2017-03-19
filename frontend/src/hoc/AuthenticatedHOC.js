import { connect } from 'react-redux'

/**
  NOTE: if using this HOC, do not use GetterHOC!
  **/

const mapStateToProps = ({ session, isAuthenticated }) => ({ session, isAuthenticated })
const mapDispatchToProps = (dispatch) => ({})

export default (component) => (
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(component)
)
